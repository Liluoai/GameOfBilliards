%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is eMQTT
%%
%% The Initial Developer of the Original Code is <ery.lee at gmail dot com>
%% Copyright (C) 2012 Ery Lee All Rights Reserved.

-module(emqtt_client).

-behaviour(gen_server2).

-export([start_link/0, go/4, info/1, make_msg/1, terminate/2, check_if_username_is_uid/1]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
    ]).

-include("emqtt.hrl").

-include("emqtt_frame.hrl").

-include("emqtt_internal.hrl").

-include_lib("elog/include/elog.hrl").
-include_lib("msgbus_common_utils/include/internal_package_pb.hrl").

-define(CLIENTIDTOIDPOOL, clientidtouidpool).
-define(HTTP_TIMEOUT, 2000).

-define(CLIENT_ID_MAXLEN, 23).
-record(state, {socket,
    conn_name,
    local_ip,
    local_port,
    await_recv,
    connection_state,
    conserve,
    parse_state,
    message_id,
    client_id,
    clean_sess,
    will_msg,
    keep_alive,
    idle_timer,
    awaiting_ack,
    subtopics,
    awaiting_rel,
    protocol_version,
    node_tag,
    uid,                    %% integer
    appkey,                 %% list
    platform,               %% interger
    mqtt_packet_recv,
    mqtt_packet_sent,
    mqtt_connect_recv,
    mqtt_conack_sent,
    mqtt_publish_recv,
    mqtt_puback_sent,
    mqtt_subscribe_recv,
    mqtt_suback_sent,
    mqtt_unsubscribe_recv,
    mqtt_unsuback_sent,
    enable_pingreq_to_mq
}).

-define(FRAME_TYPE(Frame, Type),
    Frame = #mqtt_frame{fixed = #mqtt_frame_fixed{type = Type}}).

start_link() ->
    gen_server2:start_link(?MODULE, [], []).

go(Pid, NodeTag, Sock, PingOpt) ->
    gen_server2:call(Pid, {go, NodeTag, Sock, PingOpt}).

info(Pid) ->
    gen_server2:call(Pid, info).

emqtt_client_register({_ClientId, Uid}, Pid) ->
    case ets:lookup(uid, Pid) of
        [{_, {OldPid, MRef}}] ->
            catch gen_server2:call(OldPid, duplicate_id),
            erlang:demonitor(MRef);
        [] ->
            ignore
    end,
    ProcessInfo = {Pid, erlang:monitor(process, Pid)},
    ets:insert(uid, {Uid, ProcessInfo}).

emqtt_client_unregister({ClientId, Uid}, Pid, Reason, State) ->
    case ets:lookup(uid, Uid) of
        [{_, {Pid3, _}}] ->
            if
                Pid3 == Pid ->
                    ets:delete(uid, Uid),
                    if
                      is_list(ClientId) ->
                        case Reason of
                          {shutdown,duplicate_id} ->
                            ?DEBUG("Do not send disconnect command for ~p", [Reason]),
                            ignore;
                          _ ->
                          %%todo:last will would work only if client disconnect brutly rather gentely
                            % case check_wether_MQTT_LAST_WILL_enable(Uid, State#state.appkey) of
                            %     true ->
                            %         send_MQTT_LAST_WILL_message(State);
                            %     false ->
                            %         ignore
                            % end,
                            forward_package_disconnect_to_mq(State)
                        end;
                      true ->
                        ignore
                    end;
            %%
            %% 问题同上
            %%
            %% check the comment above
            %%
                true ->
                  ?INFO("unmatched Pid, Uid ~p ", [Uid])
            end;
        [] ->
            ignore
    end.

init([]) ->
    random:seed(erlang:now()),
    eredis_pool:create_pool(pool1, 10, "127.0.0.1", 6379, 1, "abc", 100),
    {ok, undefined, hibernate, {backoff, 1000, 1000, 10000}}.

handle_call(duplicate_id, _From, State = #state{conn_name = _ConnName, client_id = _ClientId}) ->
    stop({shutdown, duplicate_id}, State);

handle_call(info, _From, #state{conn_name = ConnName,
    message_id = MsgId, client_id = ClientId} = State) ->
    Info = [{conn_name, ConnName},
        {message_id, MsgId},
        {client_id, ClientId}],
    {reply, Info, State};

handle_call({go, NodeTag, Sock, PingOpt}, _From, _State) ->
    process_flag(trap_exit, true),
    ok = throw_on_error(
        inet_error, fun() -> emqtt_net:tune_buffer_size(Sock) end),
    %FIXME: merge to registry
    emqtt_client_monitor:mon(self()),
    IdleTimer = emqtt_keep_alive:new(300000, idle_timer_timeout),
    {LIp, LPort} = case inet:sockname(Sock) of
                       {ok, {LSockIP, LSockPort}} ->
                           {emqtt_net:ip_to_int(LSockIP), LSockPort};
                       {error, Reason} ->
                           ?WARN("Get Sock IP and Port Error ~p ", [Reason]),
                           {0, 0}
                   end,
    {reply, ok,
        control_throttle(
            #state{socket = Sock,
            conn_name = undefined,
            local_ip = LIp,
            local_port = LPort,
            idle_timer = IdleTimer,
            await_recv = false,
            connection_state = running,
            conserve = false,
            parse_state = emqtt_frame:initial_state(),
            message_id = 1,
            subtopics = [],
            awaiting_ack = gb_trees:empty(),
            awaiting_rel = gb_trees:empty(),
            protocol_version = undefined,
            node_tag = NodeTag,
            uid = undefined,
            appkey = undefined,
            platform = undefined,
            mqtt_packet_recv = 0,
            mqtt_packet_sent = 0,
            mqtt_connect_recv = 0,
            mqtt_conack_sent = 0,
            mqtt_publish_recv = 0,
            mqtt_puback_sent = 0,
            mqtt_subscribe_recv = 0,
            mqtt_suback_sent = 0,
            mqtt_unsubscribe_recv = 0,
            mqtt_unsuback_sent = 0,
            enable_pingreq_to_mq = PingOpt
            })}.

udp_trace(#state{uid = Uid, client_id = ClientId} = _State, Topics, Content) ->
    emqtt_registry:trace(ClientId, Uid, Topics, Content).

handle_cast({suback, Frame}, #state{socket = Sock, protocol_version = ProtocolVersion} = State) ->
    GrantedQos = Frame#mqtt_frame.variable#mqtt_frame_suback.qos_table,
    ?DEBUG("GrantedQos ~p", [GrantedQos]),

    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?SUBACK},
        variable = #mqtt_frame_suback{
            message_id = Frame#mqtt_frame.variable#mqtt_frame_suback.message_id,
            qos_table = GrantedQos}}, ProtocolVersion),
    udp_trace(State, undefined, <<"end[server]: suback">>),
    folsom_metrics:notify({mqtt_suback_sent, {inc, 1}}),
    {noreply, State};

handle_cast({puback, Frame}, #state{socket = Sock, protocol_version = ProtocolVersion} = State) ->
    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PUBACK},
        variable = #mqtt_frame_publish{
            message_id = Frame#mqtt_frame.variable#mqtt_frame_publish.message_id}},
        ProtocolVersion),
    udp_trace(State, undefined, <<"end[server]: puback">>),
    folsom_metrics:notify({mqtt_puback_sent, {inc, 1}}),
    {noreply, State};

handle_cast({get, Frame}, #state{socket = Sock, protocol_version = ProtocolVersion} = State) ->
  Payload = Frame#mqtt_frame.payload,
  ?DEBUG("Payload ~p ~p", [Payload, Frame]),

  send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?GET},
  variable = #mqtt_frame_publish{
    message_id = Frame#mqtt_frame.variable#mqtt_frame_publish.message_id},
    payload = Payload},
    ProtocolVersion),
    udp_trace(State, undefined, <<"end[server]: get">>),
    folsom_metrics:notify({mqtt_get_sent, {inc, 1}}),
  {noreply, State};

handle_cast({unsuback, Frame}, #state{socket = Sock, protocol_version = ProtocolVersion} = State) ->
  send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?UNSUBACK},
      variable = #mqtt_frame_unsuback{
            message_id = Frame#mqtt_frame.variable#mqtt_frame_unsuback.message_id}},
       ProtocolVersion),
    udp_trace(State, undefined, <<"end[server]: unsuback">>),
    folsom_metrics:notify({mqtt_unsuback_sent, {inc, 1}}),
  {noreply, State};

handle_cast({pubrec, Frame}, #state{socket = Sock, protocol_version = ProtocolVersion} = State) ->
    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PUBREC},
        variable = #mqtt_frame_publish{
            message_id = Frame#mqtt_frame.variable#mqtt_frame_publish.message_id}},
        ProtocolVersion),
    udp_trace(State, undefined, <<"end[server]: pubrec">>),
    folsom_metrics:notify({mqtt_pubrec_sent, {inc, 1}}),
    {noreply, State};

handle_cast({pubrel, Frame}, #state{socket = Sock, protocol_version = ProtocolVersion} = State) ->
    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PUBREL},
        variable = #mqtt_frame_publish{
            message_id = Frame#mqtt_frame.variable#mqtt_frame_publish.message_id}},
        ProtocolVersion),
    udp_trace(State, undefined, <<"end[server]: pubrel">>),
    folsom_metrics:notify({mqtt_pubrel_sent, {inc, 1}}),
    {noreply, State};

handle_cast({pubcomp, Frame}, #state{socket = Sock, protocol_version = ProtocolVersion} = State) ->
    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PUBCOMP},
        variable = #mqtt_frame_publish{
            message_id = Frame#mqtt_frame.variable#mqtt_frame_publish.message_id}},
        ProtocolVersion),
    udp_trace(State, undefined, <<"end[server]: pubcomp">>),
    folsom_metrics:notify({mqtt_pubcomp_sent, {inc, 1}}),
    {noreply, State};

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

handle_info({route, Msg},
    #state{
        socket = Sock,
        protocol_version = ProtocolVersion,
        mqtt_packet_sent = _MqttPacketSent
    } = State) ->

    #mqtt_msg{retain = Retain,
    qos = Qos,
    topic = Topic,
    dup = Dup,
    message_id = MessageId,
    payload = Payload,
    encoder = Encoder} = Msg,

    Payload1 =
        if
            Encoder == undefined -> Payload;
            true -> Encoder(Payload)
        end,

    Frame = #mqtt_frame{
        fixed = #mqtt_frame_fixed{type = ?PUBLISH,
        qos = Qos,
        retain = Retain,
        dup = Dup},
        variable = #mqtt_frame_publish{topic_name = Topic,
        message_id = if
                         Qos == ?QOS_0 -> undefined;
                         true -> MessageId
                     end},
        payload = Payload1},

    send_frame(Sock, Frame, ProtocolVersion),
    udp_trace(State, Topic, <<"end[server]: publish">>),
    folsom_metrics:notify({mqtt_publish_sent, {inc, 1}}),

    if
        Qos == ?QOS_0 ->
            {noreply, State};
        true ->
            {noreply, next_msg_id(State)}
    end;

handle_info({inet_reply, _Ref, ok}, State) ->
    {noreply, State, hibernate};

handle_info({inet_async, Sock, _Ref, {ok, Data}}, #state{socket = Sock} = State) ->
    process_received_bytes(
        Data, control_throttle(State#state{await_recv = false}));

handle_info({inet_async, _Sock, _Ref, {error, Reason}}, State) ->
    network_error(Reason, State);

handle_info({inet_reply, _Sock, {error, Reason}}, State) ->
    ?ERROR("sock error: ~p~n", [Reason]),
    try
        port_close(_Sock)
    catch
        _:X ->
            ?INFO("Close Sock failed ~p", [X])
    end,
    {noreply, State};

handle_info(keep_alive_timeout, #state{keep_alive = KeepAlive} = State) ->
    TimerState = emqtt_keep_alive:state(KeepAlive),
    case TimerState of
        idle ->
            ?INFO("keep_alive timeout: ~p", [State#state.client_id]),
            case check_wether_MQTT_LAST_WILL_enable(State#state.uid, State#state.appkey) of
                true ->
                    ?DEBUG_MSG("timeout,start send MQTT_LAST_WILL"),
                    send_MQTT_LAST_WILL_message(State);
                false ->
                    ignore
            end,
            {stop, normal, State};
        active ->
            KeepAlive1 = emqtt_keep_alive:reset(KeepAlive),
            {noreply, State#state{keep_alive = KeepAlive1}};
        _ ->
            ?INFO("keep_alive timeout: ~p ~p", [State#state.client_id, TimerState])
    end;

handle_info(idle_timer_timeout, #state{idle_timer = _IdleTimer, conn_name = _ConnName} = State) ->
    {stop, normal, State};

handle_info(Info, State) ->
    {stop, {badinfo, Info}, State}.

terminate(Reason, State=#state{client_id = ClientId, keep_alive = KeepAlive, uid = Uid, idle_timer = IdleTimer}) ->
    ?DEBUG("~p ~p reason ~p", ["terminate", Uid, Reason]),
    emqtt_client_unregister({ClientId, Uid}, self(), Reason, State),
    emqtt_keep_alive:cancel(KeepAlive),
    emqtt_keep_alive:cancel(IdleTimer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

throw_on_error(E, Thunk) ->
    case Thunk() of
        {error, Reason} -> throw({E, Reason});
        {ok, Res} -> Res;
        Res -> Res
    end.

async_recv(Sock, Length, infinity) when is_port(Sock) ->
    prim_inet:async_recv(Sock, Length, -1);

async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    prim_inet:async_recv(Sock, Length, Timeout).

check_if_username_is_uid(Username) ->
    %%
    %% old style username is a random string with length 4~8 bytes
    %% now use the uid as username directly
    %%
    %% 旧的 username 是 4~8 个字节，现在直接用 uid 当作 username，
    %% 不再做 client to uid 的查询
    UsernameLen = if
                      is_list(Username) ->
                          string:len(Username);
                      is_binary(Username) ->
                          byte_size(Username)
                  end,
    if
        UsernameLen > 8 ->
            try
                ?DEBUG("Username is uid [~p]", [Username]),
                if
                    is_list(Username) ->
                        list_to_integer(Username);
                    is_binary(Username) ->
                        binary_to_integer(Username)
                end
            catch
                _:X ->
                    ?DEBUG("convert username to uid failed. ~p}", [X]),
                    undefined
            end;
        true ->
            undefined
    end.

%-------------------------------------------------------
% receive and parse tcp data
%-------------------------------------------------------
process_received_bytes(<<>>, State) ->
    {noreply, State};

process_received_bytes(Bytes,
    State = #state{parse_state = ParseState,
        conn_name = ConnStr, protocol_version = ProtocolVersion,
        mqtt_packet_recv = MqttPacketRecv
    }) ->
    try
      case emqtt_frame:parse(Bytes, ParseState, ProtocolVersion) of
          {more, ParseState1} ->
              {noreply,
                  control_throttle(State#state{parse_state = ParseState1}),
                  hibernate};
          {ok, Frame, Rest} ->
              State3 = case Frame#mqtt_frame.fixed#mqtt_frame_fixed.type of
                           ?CONNECT ->
                               State#state{
                                   client_id = Frame#mqtt_frame.variable#mqtt_frame_connect.client_id,
                                   uid = check_if_username_is_uid(Frame#mqtt_frame.variable#mqtt_frame_connect.username),
                                   protocol_version = Frame#mqtt_frame.variable#mqtt_frame_connect.proto_ver
                               };
                           _ ->
                               State
                       end,
              State2 = State3#state{mqtt_packet_recv = MqttPacketRecv + 1},
              case process_frame(Bytes, Frame, State2) of
                  {ok, State1} ->
                      PS = emqtt_frame:initial_state(),
                      process_received_bytes(
                          Rest,
                          State1#state{parse_state = PS});
                  {err, Reason, State1} ->
                      ?ERROR("MQTT protocol error ~p for connection ~p~n", [Reason, ConnStr]),
                      stop({shutdown, Reason}, State1);
                  {stop, State1} ->
                      stop(normal, State1)
              end;
          {error, Error} ->
              ?ERROR("MQTT detected framing error ~p for connection ~p~n", [ConnStr, Error]),
              stop({shutdown, Error}, State)
      end
    catch
      _:X -> ?ERROR("unpack mqtt package error ~p, ~p~n", [X,erlang:get_stacktrace()]),
      stop({shutdown, badpackage}, State)
    end.

clientid_to_uid(ClientId) ->
    case application:get_env(uid_url) of
        {ok, Url} ->
            Http_url = lists:append(Url, ClientId),
            case httpc:request(get, {Http_url, []}, [{version, "HTTP/1.0"}], []) of
                {ok, {{_Version, ReturnCode, _ReasonPhrase}, _Headers, Body}} ->
                    case ReturnCode of
                        200 ->
                            ?DEBUG("get uid from ws ~p~n", [Body]),
                            Uid = try jiffy:decode(Body) of
                                      {[{<<"uid">>, UidValue}]} -> binary_to_integer(UidValue);
                                      _ -> -3
                                  catch
                                      _ -> -3
                                  end,
                            case Uid of
                                -1 ->
                                    ?ERROR("Uid is not existed: ~p~n", [ClientId]),
                                    {error, <<"uid is not existed">>};
                                -2 ->
                                    ?ERROR("Server error: ~p~n", [Uid]),
                                    {error, <<"server error">>};
                                -3 ->
                                    ?ERROR("Invalid json return ~p of uid ~p", [Body, Uid]),
                                    {error, <<"invalid json string">>};
                                _0ther ->
                                    {ok, Uid}
                            end;
                        _Other ->
                            ?ERROR("Uncacthed case: ~p~n", [ReturnCode]),
                            {error, Body}
                    end;
                {error, REASON} ->
                    ?ERROR("get uid by clientid ~p failed ~p~n", [ClientId, REASON]),
                    {error, <<"http failed">>}
            end;
        _ ->
            {error, <<"no uid_url configured">>}
    end.

stat_mqtt(Type,
    #state{
        mqtt_connect_recv = MqttConnectRecv,
        mqtt_conack_sent = _MqttConackSent,
        mqtt_publish_recv = MqttPublishRecv,
        mqtt_puback_sent = _MqttPubackRecv,
        mqtt_subscribe_recv = MqttSubscribeRecv,
        mqtt_suback_sent = _MqttSubackSent,
        mqtt_unsubscribe_recv = MqttUnsubscribeRecv,
        mqtt_unsuback_sent = _MqtUnsubackSendt
    } =  State) ->
    case Type of
        ?CONNECT ->
            folsom_metrics:notify({mqtt_connect_recv, {inc, 1}}),
            State#state{mqtt_connect_recv = MqttConnectRecv + 1};
        ?PUBLISH ->
            folsom_metrics:notify({mqtt_publish_recv, {inc, 1}}),
            State#state{mqtt_publish_recv = MqttPublishRecv + 1};
        ?PUBACK ->
            folsom_metrics:notify({mqtt_puback_recv, {inc, 1}}),
            State;
        ?SUBSCRIBE ->
            folsom_metrics:notify({mqtt_subscribe_recv, {inc, 1}}),
            State#state{mqtt_subscribe_recv = MqttSubscribeRecv + 1};
        ?UNSUBSCRIBE ->
            folsom_metrics:notify({mqtt_unsubscribe_recv, {inc, 1}}),
            State#state{mqtt_unsubscribe_recv = MqttUnsubscribeRecv + 1};
        ?GET ->
            folsom_metrics:notify({mqtt_get_recv, {inc, 1}}),
            State;
        ?PUBREL ->
            folsom_metrics:notify({mqtt_pubrel_recv, {inc, 1}}),
            State;
        ?PUBCOMP ->
            folsom_metrics:notify({mqtt_pubcomp_recv, {inc, 1}}),
            State;
        ?PINGREQ ->
            folsom_metrics:notify({mqtt_pingreq_recv, {inc, 1}}),
            State;
        ?DISCONNECT ->
            folsom_metrics:notify({mqtt_disconnect_recv, {inc, 1}}),
            State;
        _ ->
            folsom_metrics:notify({mqtt_other_recv, {inc, 1}}),
            State
    end.

process_frame(_Bytes, Frame = #mqtt_frame{fixed = #mqtt_frame_fixed{type = Type}},
    State = #state{client_id = ClientId, keep_alive = KeepAlive,
        protocol_version = ProtocolVersion, node_tag = NodeTag,
        uid = Uid}) ->
    KeepAlive1 = emqtt_keep_alive:activate(KeepAlive),
    case validate_frame(Type, Frame) of
        ok ->
            Wether_command_message_of_MQTT_LAST_WILL = wether_command_message_of_MQTT_LAST_WILL(Frame),
            ?DEBUG("Wether_command_message_of_MQTT_LAST_WILL:~p", [Wether_command_message_of_MQTT_LAST_WILL]),
            ?DEBUG("frame from ~s: ~p", [ClientId, Frame]),
            %% TODO: configure option for switching handler
            Key = erlang:integer_to_binary(Type),
            ?DEBUG("forward to mq: key[~p]", [Key]),

            {Uid2, State2} = case application:get_env(emqtt, enable_check_register_info, true) of
                                 true ->
                                     check_register_info(Uid, ClientId, State);
                                 false ->
                                     {Uid, State}
                             end,

            Typename = emqtt_frame:type_name(Type),
            BeginHandleTraceMsg = << <<"begin[client]: ">>/binary, Typename/binary>>,

            Topics = case Type of
                ?SUBSCRIBE ->
                    Frame#mqtt_frame.variable#mqtt_frame_subscribe.topic_table;
                ?UNSUBSCRIBE ->
                    Frame#mqtt_frame.variable#mqtt_frame_subscribe.topic_table;
                _ ->
                    undefined
            end,
            emqtt_registry:trace(ClientId, Uid, Topics, BeginHandleTraceMsg),

            %%
            %% TODO: check the routing table ??? If Pid is corrent in routing table
            %%
            case {Uid2, State2} of
                {error, Error} ->
                    {error, Error};
                _ ->
                    State3 = stat_mqtt(Type, State2),
                    folsom_metrics:notify({mqtt_packet_recv, {inc, 1}}),

                    FrameBytes = emqtt_frame:serialise(Frame, ProtocolVersion),

                    handle_retained(Type, Frame),
                    EndHandleTraceMsg = << <<"end[client]: ">>/binary, Typename/binary>>,
                    HandleResult = case command_handle_locally(Type) of
                        true ->
                          {Ret,Stata4} = process_request(Type, Frame, State3#state{keep_alive = KeepAlive1}),
                          forward_package_to_mq(NodeTag, ProtocolVersion, Uid2, ClientId, FrameBytes, Key, State3),
                          {Ret,Stata4};
                        pingreq ->
                          {Ret1,State5} = process_request(Type, Frame, State3#state{keep_alive = KeepAlive1}),
                          case State5#state.enable_pingreq_to_mq of
                              true ->
                                  forward_package_to_mq(NodeTag, ProtocolVersion, Uid2, ClientId, FrameBytes, Key, State3);
                              _ ->
                                  ignore
                          end,
                          ?DEBUG("~p", [Ret1]),
                        ?DEBUG("~p", [State5]),
                          {Ret1,State5};
                        false ->
                            case Wether_command_message_of_MQTT_LAST_WILL of
                                true ->
                                    ?DEBUG_MSG("start store last will message"),
                                    store_MQTT_LAST_WILL_command_in_redis(Frame, State3),
                                    % make_package_send_to_mq(NodeTag, ProtocolVersion, Uid2, ClientId, FrameBytes, Key, State3),
                                    % {stop, State3#state{keep_alive = KeepAlive1}};
                                    {ok, State3#state{keep_alive = KeepAlive1, await_recv = false}};
                                false ->
                                    forward_package_to_mq(NodeTag, ProtocolVersion, Uid2, ClientId, FrameBytes, Key, State3),
                                    {ok, State3#state{keep_alive = KeepAlive1}}
                            end
                    end,
                    emqtt_registry:trace(ClientId, Uid, Topics, EndHandleTraceMsg),
                    HandleResult
            end;
        {error, Reason} ->
            {err, Reason, State}
    end.

forward_package_to_mq(NodeTag, ProtocolVersion, Uid2, ClientId, Bytes, Key,
    State2 = #state{
        socket = _Sock, local_ip = LIp, local_port = LPort,
        appkey = Appkey, platform = Platform}) ->

    {ok, BrokerScore} = application:get_env(emqtt, broker_score),
    InternalPackage =
    #internalpackage{
        from_ip = LIp, from_port = LPort,
        to_ip = 0, to_port = 0,
        from_tag = NodeTag,
        protocol_version = ProtocolVersion, % protocol version
        uid = Uid2, client_id = list_to_binary(ClientId),
        mqtt_package = Bytes, appkey = Appkey, platform = Platform, score = BrokerScore
    },

    BytesWithHeader = internal_package_pb:encode_internalpackage(
        InternalPackage
    ),
    case msgbus_amqp_proxy:send(Key, list_to_binary(BytesWithHeader)) of
        ok ->
            {ok, State2};
        {_, Reason1} ->
            ?CRITICAL("forward to mq failed: key[~p] ClientId[~p] Reason[~p]",
                [Key, ClientId, Reason1]),
            %% TODO remove this node from zookeeper if there no message queue connected.
            %% send notification to admininstrator
            %% TODO 如果 rabbitmq 不能正常工作，把该节点从 zookeeper 中移除
            %% 发告警给管理员
            {err, Reason1, State2};
        Else ->
            ?ERROR("Uncacthed case: ~p~n", [Else]),
            {ok, State2}
    end.

make_sure_uid(Uid, ClientId, State) ->
    case Uid of
        undefined ->
            case clientid_to_uid(ClientId) of
                {ok, Uid3} ->
                    {Uid3, State#state{uid = Uid3}};
                {error, Error} ->
                    {error, Error}
            end;
        _ -> {Uid, State}
    end.

clientid_to_appid(ClientId) ->
    ClientIdBin = list_to_binary(ClientId),
    case binary:split(ClientIdBin, <<"-">>, []) of
        [AppId, _AppAccumulationId] ->
            {ok, AppId};
        _ ->
            undefined
    end.

clientid_to_appkey(ClientId) ->
    case clientid_to_appid(ClientId) of
        {ok, AppId} ->
            case appid_to_appkey(AppId) of
                {ok, Appkey} ->
                    ?DEBUG("clientid_to_appkey ~p ~p", [ClientId, Appkey]),
                    {ok, Appkey};
                Else ->
                    Else
            end;
        _ -> error
    end.

save_clientid_to_appkey_cache(ClientId, Appkey) ->
    case clientid_to_appid(ClientId) of
        {ok, AppId} ->
            ets:insert(appid_to_appkey, {AppId, Appkey})
    end.

appid_to_appkey(AppId) ->
    case ets:lookup(appid_to_appkey, AppId) of
        [{AppId, Appkey}] ->
            {ok, Appkey};
        _ ->
            error
    end.

make_sure_appkey(Uid, ClientId, State = #state{appkey = Appkey}) ->
    case Appkey of
        undefined ->
            case ClientId of
                undefined ->
                    {ok, State};
                _ ->
                    case clientid_to_appkey(ClientId) of
                        {ok, Appkey3} ->
                            {ok, State#state{appkey = Appkey3}};
                        _ ->
                            case application:get_env(uid_to_appkey_url) of
                                {ok, Url} ->
                                    UidStr = integer_to_list(Uid),
                                    HttpURL = lists:append(Url, UidStr),
                                    case http_get_appkey(HttpURL, ?HTTP_TIMEOUT) of
                                        {ok, AppkeyBin} ->
                                            save_clientid_to_appkey_cache(ClientId, AppkeyBin),
                                            ?DEBUG("Get appkey ~p by uid ~p from reg", [AppkeyBin, Uid]),
                                            {ok, State#state{appkey = AppkeyBin}};
                                        E ->
                                            E
                                    end;
                                _ ->
                                    {error, uid_to_appkey_url_not_config}
                            end
                    end
            end;
        _ ->
            State
    end.

http_get_appkey(URL, Timeout) ->
    case http_get_request(URL, Timeout) of
        {ok, Body} ->
            try jiffy:decode(Body) of
                {[{<<"appkey">>, Appkey}]} ->
                    {ok, Appkey};
                _ ->
                    ?ERROR("appkey json result is not correct ~p", [Body]),
                    {error, Body}
            catch
                T:E ->
                    ?ERROR("Decode apppkey json failed ~p:~p", [T, E]),
                    {error, E}
            end
    end.

http_get_request(URL, Timeout) ->
    try ibrowse:send_req(URL,[{"Accept", "application/json"}],get, [], [], Timeout) of
        {ok, ReturnCode, _Headers, Body} ->
            case ReturnCode of
                "200" ->
                    ?DEBUG("http get client ~p succeed ~p", [URL, Body]),
                    {ok, Body};
                _Other ->
                    ?ERROR("http get client ~p not return 200 ~p~n", [URL, _Other]),
                    {error, Body}
            end;
        {error, connection_closing} ->
            {error, connection_closing};
        {error, REASON} ->
            ?ERROR("http get client ~p failed ~p~n", [URL, REASON]),
            {error, REASON}
    catch
        Type:Error ->
            ?ERROR("http get client ~p failed ~p:~p~n", [URL, Type, Error]),
            {error, Error}
    end.

make_sure_platform(_Uid, State = #state{platform = _Platform}) ->
    State.

command_handle_locally(?CONNECT) -> true;
command_handle_locally(?DISCONNECT) -> true;
command_handle_locally(?PINGREQ) -> pingreq;
command_handle_locally(_) -> false.

process_request(?CONNECT,
    #mqtt_frame{variable = #mqtt_frame_connect{
            username = Username,
            password = Password,
            proto_ver = ProtoVersion,
            clean_sess = _CleanSess,
            keep_alive = AlivePeriod,
            client_id = ClientId} = Var},
    #state{socket = Sock, protocol_version = ProtocolVersion,
        uid = Uid, idle_timer = IdleTimer} = State) ->
    emqtt_keep_alive:cancel(IdleTimer),

    {ReturnCode, State1} =
        case {(ProtoVersion =:= ?MQTT_PROTO_MAJOR) or (ProtoVersion =:= ?CLOS_MQTT_PROTO_MAJAR),
            valid_client_id(ClientId)} of
            {false, _} ->
                emqtt_registry:trace(ClientId, Uid, undefined, <<"bad protocol version">>),
                {?CONNACK_PROTO_VER, State};
            {_, false} ->
                emqtt_registry:trace(ClientId, Uid, undefined, <<"bad clientid">>),
                {?CONNACK_INVALID_ID, State};
            _ ->
                case emqtt_auth:check(list_to_binary(Username),
                    list_to_binary(Password), integer_to_binary(Uid)) of
                    false ->
                        emqtt_registry:trace(ClientId, Uid, undefined, <<"auth failed">>),
                        ?ERROR_MSG("MQTT login failed - no credentials"),
                        {?CONNACK_CREDENTIALS, State};
                    true ->
                        emqtt_registry:trace(ClientId, Uid, undefined, <<"auth succ">>),
                        ?INFO("connect from clientid ~s, uid ~p, keep alive ~p", [ClientId, Uid, AlivePeriod]),
                        emqtt_client_register({ClientId, Uid}, self()),

                        %% wait 2 seconds more before time out the connection
                        KeepAlive = emqtt_keep_alive:new(AlivePeriod * 1000 + 2000, keep_alive_timeout),
                        {?CONNACK_ACCEPT,
                            State#state{will_msg = make_will_msg(Var),
                            client_id = ClientId,
                            keep_alive = KeepAlive,
                            protocol_version = ProtoVersion}};
                    {error, Error} ->
                        ?ERROR("auth failed: ~p", [Error]),
                        {?CONNACK_SERVER, State}
                end
        end,

    emqtt_registry:trace(ClientId, Uid, undefined, <<"send connect to nogic">>),

    case update_broker_score(online, State) of
        ok ->
            send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?CONNACK}, variable = #mqtt_frame_connack{return_code = ReturnCode}}, ProtocolVersion);
        _ ->
            ignore
    end,

   udp_trace(State1, undefined, <<"end[server]: connack">>),
   folsom_metrics:notify({mqtt_conack_sent, {inc, 1}}),

{ok, State1};

process_request(?PUBLISH, _Frame = #mqtt_frame{
        fixed = #mqtt_frame_fixed{qos = ?QOS_0}}, State) ->
    {ok, State};

process_request(?PUBLISH,
    _Frame = #mqtt_frame{
            fixed = #mqtt_frame_fixed{qos = ?QOS_1},
            variable = #mqtt_frame_publish{message_id = MsgId}},
    State = #state{socket = Sock, protocol_version = ProtocolVersion}) ->
    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PUBACK},
    variable = #mqtt_frame_publish{message_id = MsgId}}, ProtocolVersion),
    {ok, State};

process_request(?PUBLISH,
    _Frame = #mqtt_frame{
            fixed = #mqtt_frame_fixed{qos = ?QOS_2},
            variable = #mqtt_frame_publish{message_id = MsgId}},
    State = #state{socket = Sock, protocol_version = ProtocolVersion}) ->
    put({msg, MsgId}, pubrec),
    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PUBREC},
    variable = #mqtt_frame_publish{message_id = MsgId}}, ProtocolVersion),

    {ok, State};

process_request(?PUBACK, #mqtt_frame{}, State) ->
    %TODO: fixme later
    {ok, State};

process_request(?PUBREC, #mqtt_frame{
        variable = #mqtt_frame_publish{message_id = MsgId}},
    State = #state{socket = Sock, protocol_version = ProtocolVersion}) ->
    %TODO: fixme later
    send_frame(Sock,
        #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PUBREL},
        variable = #mqtt_frame_publish{message_id = MsgId}}, ProtocolVersion),
    {ok, State};

process_request(?PUBREL,
    #mqtt_frame{
            variable = #mqtt_frame_publish{message_id = MsgId}},
    State = #state{socket = Sock, protocol_version = ProtocolVersion}) ->
    erase({msg, MsgId}),
    send_frame(Sock,
        #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PUBCOMP},
        variable = #mqtt_frame_publish{message_id = MsgId}}, ProtocolVersion),
    {ok, State};

process_request(?PUBCOMP, #mqtt_frame{
        variable = #mqtt_frame_publish{message_id = _MsgId}},
    State = #state{socket = _Sock, protocol_version = _ProtocolVersion}) ->
    %TODO: fixme later
    {ok, State};

process_request(?SUBSCRIBE,
    #mqtt_frame{
            variable = #mqtt_frame_subscribe{message_id = MessageId,
                topic_table = Topics},
            payload = undefined},
    #state{socket = Sock, protocol_version = ProtocolVersion} = State) ->

    GrantedQos = [Qos || #mqtt_topic{qos = Qos} <- Topics],

    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?SUBACK},
    variable = #mqtt_frame_suback{
        message_id = MessageId,
        qos_table = GrantedQos}}, ProtocolVersion),

    {ok, State};

process_request(?UNSUBSCRIBE,
    #mqtt_frame{
            variable = #mqtt_frame_subscribe{message_id = MessageId,
                topic_table = _Topics},
            payload = undefined},
    #state{socket = Sock, client_id = _ClientId, protocol_version = ProtocolVersion} = State) ->

    send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?UNSUBACK},
    variable = #mqtt_frame_unsuback{message_id = MessageId}}, ProtocolVersion),

    {ok, State};

process_request(?PINGREQ, #mqtt_frame{},
    #state{socket = Sock, keep_alive = KeepAlive,
        protocol_version = ProtocolVersion, uid = Uid} = State) ->
    {Ret, State1} =
        case ets:lookup(uid, Uid) of
            [{_, {Pid, _MRef}}] ->
                if
                    Pid == self() ->
                        KeepAlive1 = emqtt_keep_alive:reset(KeepAlive),
                        send_frame(Sock, #mqtt_frame{fixed = #mqtt_frame_fixed{type = ?PINGRESP}},
                            ProtocolVersion),
                        folsom_metrics:notify({mqtt_pingresp_sent, {inc, 1}}),
                        {ok, State#state{keep_alive = KeepAlive1}};
                    true ->
                        ?INFO("ets:lookup(uid, ~p): ~p, self(): ~p", [Uid, {Pid, _MRef}, self()]),
                        {ok, State}
                end;
            [] ->
                stop({shutdown, <<"No routing entry found">>}, State),
                {ok, State}
        end,
    {Ret, State1};

process_request(?DISCONNECT, #mqtt_frame{}, State = #state{client_id = ClientId}) ->
    ?INFO("~s disconnected", [ClientId]),
    {stop, State}.

next_msg_id(State = #state{message_id = 16#ffff}) ->
    State#state{message_id = 1};
next_msg_id(State = #state{message_id = MsgId}) ->
    State#state{message_id = MsgId + 1}.

make_will_msg(#mqtt_frame_connect{will_flag = false}) ->
    undefined;
make_will_msg(#mqtt_frame_connect{will_retain = Retain,
    will_qos = Qos,
    will_topic = Topic,
    will_msg = Msg}) ->
    #mqtt_msg{retain = Retain,
    qos = Qos,
    topic = Topic,
    dup = false,
    payload = Msg}.

send_will_msg(#state{will_msg = undefined}) ->
    ignore;
send_will_msg(#state{will_msg = _WillMsg}) ->
    ignore.

send_frame(Sock, Frame, ProtocolVersion) ->
    erlang:port_command(Sock, emqtt_frame:serialise(Frame, ProtocolVersion)),
    folsom_metrics:notify({mqtt_packet_sent, {inc, 1}}).

network_error(Reason,
    State = #state{conn_name = ConnStr, node_tag = _NodeTag,
        protocol_version = _ProtocolVersion,
        uid = _Uid, client_id = _ClientId}) ->
    ?INFO("MQTT detected network error '~p' for ~p", [Reason, ConnStr]),
    send_will_msg(State),
    % todo: flush channel after publish

    stop({shutdown, conn_closed}, State).

forward_package_disconnect_to_mq(State = #state{node_tag = NodeTag,
    protocol_version = ProtocolVersion, uid = Uid, client_id = ClientId}) ->
    Frame = #mqtt_frame{
        fixed = #mqtt_frame_fixed{type = ?DISCONNECT}},
    ?DEBUG("send disconnect ~p for ~p", [Frame, Uid]),
    Bytes = emqtt_frame:serialise(Frame, ProtocolVersion),
    update_broker_score(offline, State),
    forward_package_to_mq(NodeTag, ProtocolVersion, Uid, ClientId, Bytes, <<"14">>, State).

run_socket(State = #state{connection_state = blocked}) ->
    State;
run_socket(State = #state{await_recv = true}) ->
    State;
run_socket(State = #state{socket = Sock}) ->
    async_recv(Sock, 0, infinity),
    State#state{await_recv = true}.

control_throttle(State = #state{connection_state = Flow,
    conserve = Conserve}) ->
    case {Flow, Conserve} of
        {running, true} -> State#state{connection_state = blocked};
        {blocked, false} -> run_socket(State#state{
            connection_state = running});
        {_, _} -> run_socket(State)
    end.

stop(Reason, State) ->
    {stop, Reason, State}.

valid_client_id(ClientId) ->
    ClientIdLen = length(ClientId),
    1 =< ClientIdLen andalso ClientIdLen =< ?CLIENT_ID_MAXLEN.

handle_retained(?PUBLISH, #mqtt_frame{fixed = #mqtt_frame_fixed{retain = false}}) ->
    ignore;

handle_retained(?PUBLISH, #mqtt_frame{
        fixed = #mqtt_frame_fixed{retain = true},
        variable = #mqtt_frame_publish{topic_name = Topic},
        payload = <<>>}) ->
    emqtt_retained:delete(Topic);

handle_retained(?PUBLISH, Frame = #mqtt_frame{
        fixed = #mqtt_frame_fixed{retain = true},
        variable = #mqtt_frame_publish{topic_name = Topic}}) ->
    emqtt_retained:insert(Topic, make_msg(Frame));

handle_retained(_, _) ->
    ignore.

validate_frame(?PUBLISH, #mqtt_frame{variable = #mqtt_frame_publish{topic_name = Topic}}) ->
    case emqtt_topic:validate({publish, Topic}) of
        true -> ok;
        false -> {error, badtopic}
    end;

validate_frame(?UNSUBSCRIBE, #mqtt_frame{variable = #mqtt_frame_subscribe{topic_table = Topics}}) ->
    ErrTopics = [Topic || #mqtt_topic{name = Topic, qos = _Qos} <- Topics,
        not emqtt_topic:validate({subscribe, Topic})],
    case ErrTopics of
        [] -> ok;
        _ -> ?ERROR("error topics: ~p", [ErrTopics]), {error, badtopic}
    end;

validate_frame(?SUBSCRIBE, #mqtt_frame{variable = #mqtt_frame_subscribe{topic_table = Topics}}) ->
    ErrTopics = [Topic || #mqtt_topic{name = Topic, qos = Qos} <- Topics,
        not (emqtt_topic:validate({subscribe, Topic}) and (Qos < 3))],
    case ErrTopics of
        [] -> ok;
        _ -> ?ERROR("error topics: ~p", [ErrTopics]), {error, badtopic}
    end;

validate_frame(_Type, _Frame) ->
    ok.

make_msg(#mqtt_frame{
        fixed = #mqtt_frame_fixed{qos = Qos,
            retain = Retain,
            dup = Dup},
        variable = #mqtt_frame_publish{topic_name = Topic,
            message_id = MessageId},
        payload = Payload}) ->
    #mqtt_msg{retain = Retain,
    qos = Qos,
    topic = Topic,
    dup = Dup,
    message_id = MessageId,
    payload = Payload}.

update_broker_score(Action, State) ->
    case application:get_env(emqtt, enable_update_broker_score) of
        {ok, true} ->
            Uid = State#state.uid,
            ClientId = State#state.client_id,
            NodeTag = State#state.node_tag,
            {ok, BrokerScore} = application:get_env(emqtt, broker_score),
            {ok, EtopicfsPostURL} = application:get_env(emqtt, etopicfs_post_url),
            {ok, RequestEtopicfsTimeout} = application:get_env(emqtt, request_etopicfs_timeout),

            ?DEBUG("Action:~p, EtopicfsPostURL:~p, Uid:~p, ClientId:~p, NodeTag:~p, BrokerScore:~p, RequestEtopicfsTimeout:~p~n",
                [Action, EtopicfsPostURL, Uid, ClientId, NodeTag, BrokerScore, RequestEtopicfsTimeout]),

            Parm = {[
                        {<<"action">>, emqtt_utils:to_bin(Action)},
                        {<<"cid">>, emqtt_utils:to_bin(ClientId)},
                        {<<"topic">>, <<"">>},
                        {<<"uid">>, Uid},
                        {<<"tag">>, emqtt_utils:to_bin(NodeTag)},
                        {<<"score">>, emqtt_utils:to_bin(BrokerScore)}
            ]},
            JSONBody = jiffy:encode(Parm),

            case emqtt_utils:http_post(EtopicfsPostURL, JSONBody, RequestEtopicfsTimeout) of
                {ok, _Body} ->
                    ok;
                {error, Reason} ->
                    ?ERROR("update broker score failed, reason:~p~n", [Reason]),
                    {error, Reason}
            end;
        _ ->
            ok
    end.

check_register_info(Uid, ClientId, State) ->
    {Uid2, State20} = case ClientId of
                          undefined ->
                              {error, <<"undefined clientid">>};
                          _ ->
                              make_sure_uid(Uid, ClientId, State)
                      end,
    State30 = case make_sure_appkey(Uid, ClientId, State20) of
                  {ok, State21} ->
                      ?DEBUG_MSG("make sure appkey succ"),
                      State21;
                  _ ->
                      State20
              end,
    State2 = case make_sure_platform(Uid, State30) of
                 {ok, State31} ->
                     ?DEBUG_MSG("make sure platform succ"),
                     State31;
                 _ ->
                     State30
             end,
    {Uid2, State2}.


store_MQTT_LAST_WILL_command_in_redis(Frame, State = #state{appkey = Appkey, uid = Uid}) ->

    {Topic, Payload, Qos, Retain} = get_detail_message_of_MQTT_LAST_WILL(Frame),
    Unique_id = lists:concat([Appkey, Uid]),
    ?DEBUG("~p", [Unique_id]),
    eredis_pool:q(pool1, ["HSET", Unique_id, "topic", Topic]),
    eredis_pool:q(pool1, ["HSET", Unique_id, "payload", Payload]),
    eredis_pool:q(pool1, ["HSET", Unique_id, "qos", Qos]),
    eredis_pool:q(pool1, ["HSET", Unique_id, "retain", Retain]),
    ok.

get_detail_message_of_MQTT_LAST_WILL(Frame) ->
    Detail_message_of_MQTT_LAST_WILL = binary_to_list(Frame#mqtt_frame.payload),
    %add erro handle
    ?DEBUG("~p", [Detail_message_of_MQTT_LAST_WILL]),
    Result = string:tokens(Detail_message_of_MQTT_LAST_WILL, "|"),
    ?DEBUG("~p", [Result]),

%Topic,Payload,Qos,Retain all are list
%todo:make sure:qos is integer, qos is true or false
    Details = case string:tokens(Detail_message_of_MQTT_LAST_WILL, "|") of
        [Topic, Payload, Qos, Retain] ->
            Is_topic = is_list(Topic),
            Is_payload = is_list(Payload),
              Is_qos = is_list(Qos),
             Is_retain = is_list(Retain),
             ?DEBUG("~p",[Topic]),
            ?DEBUG("~p",[Payload]),
             ?DEBUG("~p",[Qos]),
             ?DEBUG("~p",[Retain]),

             ?DEBUG("~p",[Is_topic]),
             ?DEBUG("~p",[Is_payload]),
             ?DEBUG("~p",[Is_qos]),
             ?DEBUG("~p",[Is_retain]),
             {Topic, Payload, Qos, Retain};
        _Else ->
        ?DEBUG("Analysis MQTT_LAST_WILL payload erro, wrong formate of payload:~p", [Detail_message_of_MQTT_LAST_WILL]),
        wrong
    end,
    Details.



wether_command_message_of_MQTT_LAST_WILL(Frame) ->
    Wether_command_message_of_MQTT_LAST_WILL = case is_record(Frame#mqtt_frame.variable, mqtt_frame_publish) of
        true ->
            case Frame#mqtt_frame.variable#mqtt_frame_publish.topic_name == "MQTT_LAST_WILL" of
                true ->
                    true;
                _ ->
                    false
                end;
        _ ->
            false
    end,
    Wether_command_message_of_MQTT_LAST_WILL.

check_wether_MQTT_LAST_WILL_enable(Uid, Appkey) ->
    Unique_id = lists:concat([Appkey, Uid]),
    {ok, Result} = eredis_pool:q(pool1, ["HLEN", Unique_id]),
    ?DEBUG("Result:~p", [Result]),
    Wether_MQTT_LAST_WILL_enable = case eredis_pool:q(pool1, ["HLEN", Unique_id]) of
        {ok, <<"4">>} ->
            true;
        _ ->
            false
    end,
    ?DEBUG("Wether_MQTT_LAST_WILL_enable:~p", [Wether_MQTT_LAST_WILL_enable]),
    Wether_MQTT_LAST_WILL_enable.
               
send_MQTT_LAST_WILL_message(State) ->
    {Topic, Payload, Qos, Retain} = get_MQTT_LAST_WILL_message_from_redis(State),
    ?DEBUG_MSG("here"),

    forward_MQTT_LAST_WILL_package_to_mq(Topic, Payload, Qos, Retain, State),




%%todo:change type, <<"3">> to variable

    % Mq_package = make_package_send_to_mq(State),
    send_packege.




% make_package_send_to_mq(NodeTag, ProtocolVersion, Uid2, ClientId, FrameBytes, Key, State) ->
%     Mqtt_frame = #mqtt_frame{fixed = #mqtt_frame_fixed{type = 3, dup = false, qos = 1, retain = false}, 
%                              variable = #mqtt_frame_publish{topic_name = "test_MQTT_LAST_WILL_topic", message_id = 12407714287953586446},
%                              payload = <<"test_MQTT_LAST_WILL_payload">>},
%     FrameBytes2 = emqtt_frame:serialise(Mqtt_frame, 19),
%     ?DEBUG("~p", [Key]),
%     forward_package_to_mq(NodeTag, ProtocolVersion, Uid2, ClientId, FrameBytes2, Key, State).

get_MQTT_LAST_WILL_message_from_redis(State = #state{appkey = Appkey, uid = Uid}) ->

?DEBUG_MSG("here"),
    Unique_id = lists:concat([Appkey, Uid]),
    {ok, Topic} = eredis_pool:q(pool1, ["HGET", Unique_id, "topic"]),
    {ok, Payload} = eredis_pool:q(pool1, ["HGET", Unique_id, "payload"]),
    {ok, Qos} = eredis_pool:q(pool1, ["HGET", Unique_id, "qos"]),
    {ok, Retain} = eredis_pool:q(pool1, ["HGET", Unique_id, "retain"]),
?DEBUG_MSG("here"),
%todo :what if delete failed??
    {ok, Delte_success} = eredis_pool:q(pool1, ["DEL", Unique_id]),
?DEBUG_MSG("here"),

    ?DEBUG("~p",[Unique_id]),
    ?DEBUG("~p",[Topic]),
    ?DEBUG("~p",[Payload]),
    ?DEBUG("~p",[Qos]),
    ?DEBUG("~p",[Retain]),

    Topic_in_list = binary_to_list(Topic),
    Qos_in_integer = binary_to_integer(Qos),
    Retain_in_atom = binary_to_atom(Retain, latin1),
    ?DEBUG("~p",[Topic_in_list]),
    ?DEBUG("~p",[Qos_in_integer]),
    ?DEBUG("~p",[Retain_in_atom]),
?DEBUG_MSG("here"),

    {Topic_in_list, Payload, Qos_in_integer, Retain_in_atom}.

forward_MQTT_LAST_WILL_package_to_mq(Topic, Payload, Qos, Retain, State = #state{uid = Uid , node_tag = Node_tag, protocol_version = Protocol_version, client_id = Client_id})->
%%message_id ???????
    Mqtt_frame = #mqtt_frame{fixed = #mqtt_frame_fixed{type = 3, dup = false, qos = Qos, retain = Retain}, 
                             variable = #mqtt_frame_publish{topic_name = Topic, message_id = 12407714287953586446},
                             payload = Payload},
    ?DEBUG("~p", [Mqtt_frame]),
    FrameBytes = emqtt_frame:serialise(Mqtt_frame, Protocol_version),

    ?DEBUG_MSG("here"),

    Result = forward_package_to_mq(Node_tag, Protocol_version, Uid, Client_id, FrameBytes, <<"3">>, State),
    %%todo:what if forward_package failt?? learn it from other place.
    ?DEBUG("~p", [Result]).
