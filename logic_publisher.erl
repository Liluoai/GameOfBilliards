%%%-------------------------------------------------------------------
%%% @author zhanghu
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 十月 2014 下午2:51
%%%-------------------------------------------------------------------
-module(logic_publish).
-author("zhanghu").

%% API
-export([send_online_notify/6,
         send_message_to_user_by_mid/7,
         send_recvack_to_user_by_mid/6,
         handle_publish_package/8,
         send_offline_notify/6,
         send_to_front/2,
         send_pubrel/4,
         do_publish/11,
         do_presence_publish/9,
         handle_publish_for_topic/12,
         send_apns_by_uid/3, % exported only for test.
         send_to_apns/1, % exported only for test.
         publish_to_alias/12, % exported only for test.
         alias_publish_to_one_uid/12,
         send_puback_or_publish2_ack/6,% exported only for test.
         publish_to_one_uid/9,% exported only for test.
         send_online_offline_notify/7,% exported only for test.
         do_publish_to_ios/3,% exported only for test.
         send_publish_command_to_one_uid/10,
         do_publish_to_ios_by_topic/5,
         send_ext_command_to_one_uid/8,
         handle_alias_offline_message/7,
         async_publish/12
         ]).

-compile({parse_transform, lager_transform}).

-include("mqtt_frame.hrl").
-include("esockproxy_mqtt.hrl").
-include_lib("elog/include/elog.hrl").
-include_lib("emqtt/include/emqtt.hrl").
-include_lib("emqtt/include/emqtt_frame.hrl").
-include_lib("msgbus_common_utils/include/internal_package_pb.hrl").


%%%===================================================================
%%%  wrapper
%%%===================================================================
send_to_apns(ApnsPackage) ->
    %% TODO: configerable routing key
    RoutingKey = <<"apns_routing_key">>,
    msgbus_amqp_proxy:send(RoutingKey, ApnsPackage).

send_to_front(<<?SOCKPROXY_NODE_TAG>>, InternalPackage) ->
    case esockproxy_client:handle_internal_package(InternalPackage) of
        table_not_exist ->
            ?DEBUG_MSG("socketio isn't start for this node, sendto message_queue"),
            RoutingKey = << ?FRONTKEYPREFIX/binary, <<?SOCKPROXY_NODE_TAG>>/binary >>,
            BytesWithHeader = internal_package_pb:encode_internalpackage(
                InternalPackage
            ),
            msgbus_amqp_proxy:send(RoutingKey, list_to_binary(BytesWithHeader));
        _ ->
            ok
    end;

send_to_front(FrontTagBin, InternalPackage) ->
    RoutingKey = << ?FRONTKEYPREFIX/binary, FrontTagBin/binary >>,
    BytesWithHeader = internal_package_pb:encode_internalpackage(
        InternalPackage
    ),
    msgbus_amqp_proxy:send(RoutingKey, list_to_binary(BytesWithHeader)).

send_puback(UidBin, MessageId, FrontTagBin, ProtocolVersion) ->
    InternalPackage = mqtt_package:form_internal_package_puback(FrontTagBin, UidBin, MessageId, ProtocolVersion),
    folsom_metrics:notify({mqtt_puback_send, {inc, 1}}),
    send_to_front(FrontTagBin, InternalPackage).

%% send puback or extcmd: publish2_ack
send_puback_or_publish2_ack(PublisherUidBin, MessageId, QoS, TagBin, Opts, ProtocolVersion) ->
    Mid = logic_util:make_sure_binary(MessageId),
    if Opts =:= null ->
            send_puback(PublisherUidBin, MessageId, TagBin, ProtocolVersion);
        true ->
            send_ext_command_to_one_uid(PublisherUidBin, Mid, ?SUCCESS, QoS,
                TagBin, ?CMD_PUBLISH2_ACK, 0, ProtocolVersion)
    end.

send_pubrec(UidBin, MessageId, FrontTagBin, ProtocolVersion) ->
    InternalPackage = mqtt_package:form_internal_package_pubrec(FrontTagBin, UidBin, MessageId, ProtocolVersion),
    folsom_metrics:notify({mqtt_pubrec_send, {inc, 1}}),
    send_to_front(FrontTagBin, InternalPackage).

send_pubrel(UidBin, MessageId, FrontTagBin, ProtocolVersion) ->
    InternalPackage = mqtt_package:form_internal_package_pubrel(FrontTagBin, UidBin, MessageId, ProtocolVersion),
    folsom_metrics:notify({mqtt_pubrel_send, {inc, 1}}),
    send_to_front(FrontTagBin, InternalPackage).

send_pubcomp(UidBin, MessageId, FrontTagBin, ProtocolVersion) ->
    InternalPackage = mqtt_package:form_internal_package_pubcomp(FrontTagBin, UidBin, MessageId, ProtocolVersion),
    folsom_metrics:notify({mqtt_pubcomp_send, {inc, 1}}),
    send_to_front(FrontTagBin, InternalPackage).

%%%===================================================================
%%%  publish to topic
%%%===================================================================

%% Do publish to Topic, won't do publish to apns
do_publish(AppkeyBin, Appid, PublisherUidBin, SenderId, QoS, Topic, Payload, Opts, TagBin, ProtocolVersion, State=#state{etopicfs_get_url=EtopicfsGetUrl}) ->
    case logic_topicfs:get_uid_list_index_from_topicfs(AppkeyBin, Topic, EtopicfsGetUrl) of
        {ok, UidListIndex} ->
            publish_to_topic_slice(UidListIndex, AppkeyBin, Appid, SenderId, QoS, Topic, Payload, true, 0, ProtocolVersion, State),
            case QoS of
                ?QOS_1 ->
                    send_puback_or_publish2_ack(PublisherUidBin, SenderId, QoS, TagBin, Opts, ProtocolVersion);
                ?QOS_2 ->
                    send_pubcomp(PublisherUidBin, SenderId, TagBin, ProtocolVersion);
                _Other ->
                    ignore
            end;
        _Else2 ->
            ignore
    end.

async_publish(
    AppkeyBin, AppidBin, PublisherUidBin, SenderId,
    QoS, Topic, Payload, Opts,
    TagBin, ProtocolVersion, State=#state{etopicfs_get_url=EtopicfsGetUrl},
    Ack) ->
    case logic_publish_fsm_monitor_sup:start_publish(undefined, [SenderId, EtopicfsGetUrl, AppkeyBin, AppidBin,
        Topic, Payload, TagBin, QoS, ProtocolVersion, State]) of
        {ok, _} ->
            if Ack ->
                    case QoS of
                        ?QOS_1 ->
                            send_puback_or_publish2_ack(PublisherUidBin, SenderId, QoS, TagBin, Opts, ProtocolVersion);
                        ?QOS_2 ->
                            send_pubcomp(PublisherUidBin, SenderId, TagBin, ProtocolVersion);
                        _Other ->
                            ignore
                    end;
                true ->
                    ignore
            end,
            ok;
        Error ->
            ?ERROR("publish ~p by slice worker failed: ~p", [SenderId, Error]),
            error
    end.

do_publish_to_ios_by_topic(AppkeyBin, Topic, Payload, Opts, EtopicfsGetUrl) ->
    case lists:suffix("/p", Topic) of
        true ->
            ignore;
        _ ->
            send_to_apns_by_topic(AppkeyBin, Topic, Payload, Opts, EtopicfsGetUrl)
    end.


send_to_apns_by_topic(AppkeyBin, Topic, Payload, Opts, EtopicfsGetUrl) ->
    TopicPlatform = logic_util:replace_topic_hash_sign(Topic) ++ "/1",
    case logic_topicfs:get_uid_list_index_from_topicfs(AppkeyBin, TopicPlatform, EtopicfsGetUrl) of
        {ok, UidListIndex} ->
            apns_to_topic_slice(UidListIndex, Payload, Opts, EtopicfsGetUrl);
        _ ->
            ignore
    end.

apns_to_topic_slice([], _Payload, Opts, _URL) ->
    ?DEBUG("apns publish,  Payload ~p, Opts ~p", [_Payload, Opts]),
    ignore;

apns_to_topic_slice([H|T], Payload, Opts, EtopicfsGetURL) ->
    apns_to_topic(H, Payload, Opts, EtopicfsGetURL),
    apns_to_topic_slice(T, Payload, Opts, EtopicfsGetURL).


%% TODO: support mutli add messageId to message queue
apns_to_topic(UidListFile,  Payload, Opts, EtopicfsGetURL) ->
    case logic_topicfs:get_uid_list_from_topicfs(logic_util:to_string(UidListFile), EtopicfsGetURL) of
        {ok, UidList} ->
            do_publish_to_ios(UidList, Payload, Opts);
        _Else ->
            0
    end.

do_publish_to_ios([], _Payload, _Opts) ->
    ok;

do_publish_to_ios([UidBin|UidList], Payload, Opts) ->
    send_apns_by_uid(UidBin, Payload, Opts),
    do_publish_to_ios(UidList, Payload, Opts).

send_apns_by_uid(UidBin, Payload, Opts) ->
    Now = 0,
    Uid = logic_util:to_integer(UidBin),

    if  Opts =:= null ->
        PayloadLen = byte_size(Payload),
        Package = <<Uid:64, Now:32, PayloadLen:16, Payload/binary>>,
        send_to_apns(Package);
        true ->
            ApnJsonData = logic_extcmd:get_apnjson_from_opts(Opts),
            ?DEBUG("Apn Json ~p for Uid ~n", [ApnJsonData]),
            if ApnJsonData =/= ?NONE_BIN_STR ->
                Unkown = 0,
                Ver = 2,
                Len = byte_size(ApnJsonData),
                Package = <<Uid:64, Now:32, Unkown:16, Ver:8, Len:16, ApnJsonData/binary>>,
                send_to_apns(Package);
                true ->
                    ?DEBUG_MSG("send opts without apn json~n"),
                    ignore
            end
    end.

%%publish_to_platform(AppkeyBin, Appid, SenderId, QoS, Topic, Payload, Platform, Opts) ->
%%    TopicPlatform = Topic ++ "/" ++ logic_util:to_string(Platform),
%%    case logic_topicfs:get_uid_list_index_from_topicfs(AppkeyBin, TopicPlatform) of
%%        {ok, UidListIndex} ->
%%            publish_to_topic_slice(UidListIndex, AppkeyBin, Appid, SenderId, QoS, TopicPlatform, Payload, false, 0, Opts);
%%        _ ->
%%            ignore
%%    end.


%% TODO: for presence publish, TopicFlag is true, will stat presence publish
%% TopicFlag: true -> normal topic, false -> special topic
publish_to_topic_slice([], Appkey, _Appid, SenderId, QoS, Topic, _Payload, TopicFlag, Count, _ProtocolVersion, _State=#state{ttl=TTL}) ->
    ?DEBUG("publish ~p uids, Appkey ~p, Topic ~p, MessageId ~p, Qos ~p", [Count, Appkey, Topic, SenderId, QoS]),
    case TopicFlag of
        true when Count>0, QoS > ?QOS_0 ->
            logic_storage:stat_publish_subs_count(SenderId, Count, TTL);
        _ ->
            ok
    end;

publish_to_topic_slice([H|T], Appkey, Appid, SenderId, QoS, Topic, Payload, TopicFlag, Count, ProtocolVersion, State) ->
    ?DEBUG("start  ~p", [State]),
    UidCount = publish_to_topic(H, Appkey, Appid, SenderId, QoS, Topic, Payload, TopicFlag, ProtocolVersion, State),
    publish_to_topic_slice(T, Appkey, Appid, SenderId, QoS, Topic, Payload, TopicFlag, Count+UidCount, ProtocolVersion, State).

%% TODO: support mutli add messageId to message queue
publish_to_topic(UidListFile, Appkey, _Appid, SenderId, QoS, Topic, Payload, _TopicFlag, ProtocolVersion, State=#state{ttl=TTL, etopicfs_get_url=EtopicfsGetURL}) ->
    case logic_topicfs:get_uid_list_from_topicfs(logic_util:to_string(UidListFile), EtopicfsGetURL) of
        {ok, UidList} ->
            case QoS of
                ?QOS_0 ->  %% QoS 0 won't save messageId
                    publish_to_uid_list(UidList, Appkey, SenderId, QoS, Topic, Payload, TTL, ProtocolVersion, State);
                _Else ->
                    save_publish_to_uid_list(UidList, Appkey, SenderId, QoS, Topic, Payload, ProtocolVersion, State)
            end,
            logic_third_party_push:third_party_push(UidList, Appkey, Payload),
            length(UidList);
        _Else ->
            0
    end.

save_publish_to_uid_list([], _Appkey, _MessageId, _QoS, _Topic, _Payload, _ProtocolVersion, _State) ->
    ok;

save_publish_to_uid_list([Uid|T], Appkey, MessageId, QoS, Topic, Payload, ProtocolVersion,
                         State=#state{msg_queue_ttl = MSG_QUEUE_TTL, ttl=TTL, msg_queue_version=MSGQ_VERSION, max_message_queue_len = MaxLen}) ->
    AppkeyBin = logic_util:make_sure_binary(Appkey),
    logic_storage:save_offline_message_id_to_queue(Uid, integer_to_binary(MessageId), MSGQ_VERSION,
        MSG_QUEUE_TTL, MaxLen, AppkeyBin),
    publish_to_one_uid(Appkey, Uid, MessageId, Topic, Payload, QoS, TTL, ProtocolVersion, State),
    save_publish_to_uid_list(T, Appkey, MessageId, QoS, Topic, Payload, ProtocolVersion, State).

publish_to_uid_list([], _Appkey, _MessageId, _QoS, _Topic, _Payload, _TTL, _ProtocolVersion, _State) ->
    ok;

publish_to_uid_list([Uid|T], Appkey, MessageId, QoS, Topic, Payload, TTL, ProtocolVersion, State) ->
    publish_to_one_uid(Appkey, Uid, MessageId, Topic, Payload, QoS, TTL, ProtocolVersion, State),
    publish_to_uid_list(T, Appkey, MessageId, QoS, Topic, Payload, TTL, ProtocolVersion, State).

publish_to_one_uid(Appkey, Uid, MessageId, Topic, Payload, QoS, TTL, ProtocolVersion, State) ->
    case logic_storage:get_route_table_item(Uid, State#state.route_table_storage) of
        {ok, {Flag, _Ip, _Port, Tag}} ->
            case Flag == ?ONLINE of
                true ->
                    send_publish_command_to_one_uid(Appkey, Tag, Uid, MessageId, Topic, Payload, QoS, TTL, ProtocolVersion, State);
                _ ->
                    ignore
            end;
        _Else ->
            ignore
    end.

send_publish_command_to_one_uid(Appkey, FrontTag, Uid, MessageId, Topic, Payload, QoS, TTL, ProtocolVersion, State) ->
    InternalPackage = mqtt_package:form_internal_package_publish(FrontTag, Uid, Topic, QoS, MessageId, Payload, ProtocolVersion),
    %% TODO: refactory following codes. NeedSend? NeedStat?
    case lists:suffix("/p", Topic) of
        true ->
            send_to_front(FrontTag, InternalPackage);
        _ ->
            case lists:prefix("$$", Topic) of
                true ->
                    ignore;
                _ ->
                    Result = send_to_front(FrontTag, InternalPackage),
                    case lists:prefix(",yaliget", Topic) of
                        true ->
                            ignore;
                        _ ->
                            case lists:keyfind(online, 1, State#state.stat_flow_cmd) of
                                {online, IsOnlineEnable} ->
                                    logic_storage:stat_publish_online_count(Appkey, TTL, State#state.stat_publish_time, IsOnlineEnable);
                                _ ->
                                    logic_storage:stat_publish_online_count(Appkey, TTL, State#state.stat_publish_time, true)
                            end
                    end,
                    Result
            end
    end.

%%%===================================================================
%%%  extend command
%%%===================================================================
send_ext_command_to_one_uid(Uid, MessageId, Payload, QoS, FromTag, ExtCmd, ExtStatus, ProtocolVersion) ->
    Mid = logic_util:make_sure_binary(MessageId),
    InternalPackage
        = mqtt_package:form_internal_package_extcmd(FromTag, Uid, QoS, binary_to_integer(Mid),
                                                            Payload, ExtCmd, ExtStatus, ProtocolVersion),
    ?DEBUG("send extend command to uid ~p ~p ~p ~p ~p", [Uid, MessageId, Payload, ExtCmd, ExtStatus]),
    folsom_metrics:notify({mqtt_ext_send, {inc, 1}}),
    send_to_front(FromTag, InternalPackage).


%%%===================================================================
%%% presence
%%%===================================================================
do_topic_list_presence([], _Appkey, _Appid, _Uid, _Alias, _FromTag, _Action, _ProtocolVersion, _State) ->
    ok;
do_topic_list_presence([H | T], Appkey, Appid, Uid, Alias, FromTag, Action, ProtocolVerson, State) ->
    do_presence_publish(H, Appkey, Appid, Uid, Alias, FromTag, Action, ProtocolVerson, State),
    do_topic_list_presence(T, Appkey, Appid, Uid, Alias, FromTag, Action, ProtocolVerson, State).

do_presence_publish(Topic, Appkey, Appid, Uid, Alias, FromTag, Action, ProtocolVersion, State) ->
    case State#state.enable_presence of
        true ->
            case appkey_no_presence(Appkey) of
                false ->
                    PresenceTopic = logic_util:to_string(Topic) ++ "/p",
                    Payload = generate_presence_publish_json(Action, Alias),
                    do_publish(Appkey, Appid, Uid, 0, ?QOS_0, PresenceTopic, Payload, null, FromTag, ProtocolVersion, State),
                    ok;
                _ ->
                    appkey_presence_disabled
            end;
        false ->
            ignore

    end.

appkey_no_presence(Appkey) ->
    case ets:lookup(?NO_PRESENCE_ETS, Appkey) of
        [{Appkey}] -> true;
        [] -> false;
        _ -> false
    end.

generate_presence_publish_json(Action, Alias) ->
    PayloadItem = {[
        {<<"action">>, Action},
        {<<"alias">>, logic_util:make_sure_binary(Alias)},
        {<<"timestamp">>, logic_util:get_millisec()}
    ]},
    Payload = jiffy:encode(PayloadItem),
    Payload.


send_online_notify(Appkey, Appid, Uid, FromTag, ProtocolVersion, State) ->
    case State#state.enable_presence of
        true ->
            send_online_offline_notify(Appkey, Appid, Uid, FromTag, online, ProtocolVersion, State);
        _ ->
            ignore
    end.

send_offline_notify(Appkey, Appid, Uid, FromTag, ProtocolVersion, State) ->
    case State#state.enable_presence of
        true ->
            send_online_offline_notify(Appkey, Appid, Uid, FromTag, offline, ProtocolVersion, State);
        _ ->
            ignore
    end.

send_online_offline_notify(Appkey, Appid, Uid, FromTag, Action, ProtocolVersion, State=#state{etopicfs_get_url=EtopicfsGetURL}) ->
    case logic_storage:get_alias_from_cb(Uid) of
        {ok, Alias} ->
            case logic_topicfs:get_topic_list_from_topicfs(Uid, EtopicfsGetURL) of
                {ok, TopicList} ->
                    do_topic_list_presence(TopicList, Appkey, Appid, Uid, Alias, FromTag, Action, ProtocolVersion, State);
                _Else ->
                    ignore
            end;
        Else ->
            ?DEBUG("Get alias from Uid ~p failed: ~p", [Uid, Else])
    end.

%%%===================================================================
%%% offline message
%%%===================================================================

%%MessageId: binary
send_message_to_user_by_mid(Appkey, Appid, Uid, MessageId, FromTag, ProtocolVersion, State=#state{ttl=TTL, msg_queue_version = MSGQ_VERSION}) ->
    ?DEBUG("MessageId ~p Appid ~p", [MessageId, Appid]),
    case logic_storage:get_offline_message_body(Appkey, Appid, MessageId) of
        {error, _Error} ->
            ?DEBUG("get offline message body failed ~p", [_Error]),
            logic_storage:remove_offline_message_id(Uid, MessageId, MSGQ_VERSION, logic_util:make_sure_binary(Appkey)),
            {error, _Error};
        {ok, {TopicBin, PayloadBin, QoSBin, _Publisher}} ->
            ?DEBUG("Send message MessageId ~p, Payload ~p to Uid ~p", [MessageId, PayloadBin, Uid]),
            TopicName = binary_to_list(TopicBin),
            Topic = case lists:prefix(",yta", TopicName) of
                true ->
                    Index = string:len(",yta/") + 1,
                    string:substr(TopicName, Index);  %% remove ,yta/ prefix
                _ ->
                    TopicName
            end,
            send_publish_command_to_one_uid(Appkey, FromTag, Uid, logic_util:to_integer(MessageId),
                                            Topic, PayloadBin,
                                            logic_util:to_integer(QoSBin), TTL, ProtocolVersion, State)
    end.

%%%===================================================================
%%% recvack message
%%%===================================================================

%%RecvAckMsgId: binary
send_recvack_to_user_by_mid(AppkeyBin, Appid, Uid, RecvAckMsgId, FromTag, ProtocolVersion) ->
    Result = logic_storage:get_offline_recvack_message_body(<<"puback_">>, AppkeyBin, Appid, RecvAckMsgId),
    case Result of
        {ok, MessageMeta3} ->
            ?DEBUG("send_recvack_to_uid ~p ~p ~p ~p", [Appid, Uid, RecvAckMsgId, MessageMeta3]),
            send_ext_command_to_one_uid(Uid, RecvAckMsgId, MessageMeta3, 0, FromTag, ?CMD_RECVACK, 0, ProtocolVersion);
        _Else3 ->
            _Else3
    end.


%%%===================================================================
%%%  publish for different topics
%%%===================================================================

%% Appkey, Uid, FromTag -> binary
handle_publish_package(AppkeyBin, PublisherUidBin, ClientId, Platform, FromTagBIn, ProtocolVersion, Frame, State) ->
    #mqtt_frame{
        fixed = #mqtt_frame_fixed{qos = QoS, retain = _Retain, dup = _Dup},
        variable = #mqtt_frame_publish{topic_name = Topic, message_id = MessageId}, payload = Payload} = Frame,

    ?DEBUG("Uid ~p  publish ~p to topic ~p, MessageId ~p QoS ~p, Appkey ~p", [PublisherUidBin, Payload, Topic, MessageId, QoS, AppkeyBin]),
    %store message in mysql
    logic_publish_history_storage:store_publish_history(PublisherUidBin, MessageId, AppkeyBin, QoS, Topic, Payload),
    handle_publish_for_topic(AppkeyBin, PublisherUidBin, ClientId, Topic, Platform, Payload, QoS, MessageId, null, FromTagBIn, ProtocolVersion, State).

handle_set_alias(AppkeyBin, ClientId, PublisherUidBin, Payload, MessageId, FromTagBin, ProtocolVersion, State) ->
    case set_alias(AppkeyBin, PublisherUidBin, Payload, MessageId, FromTagBin, ProtocolVersion) of
        ok ->
            case State#state.enable_alias_offline_msg of
                true ->
                    case logic_util:clientid_to_appid(ClientId) of
                        {ok, Appid} ->
                            handle_alias_offline_message(AppkeyBin, Payload, Appid, PublisherUidBin, FromTagBin, ProtocolVersion, State);
                        Else ->
                            logic_trace:trace(ClientId, undefined, undefined, <<"get appid from clientid failed">>),
                            ?ERROR("get appid from clientid failed ~p", [Else])
                    end;
                _ ->
                    ignore
            end;
        _ ->
            ?ERROR("Set alias failed ~p ~p", [AppkeyBin, Payload])
    end.

handle_publish_for_topic(AppkeyBin, PublisherUidBin, ClientId, Topic, Platform, Payload, QoS, MessageId, Opts, FromTagBin, ProtocolVersion, State=#state{ttl=TTL}) ->
    case Topic of
        ",yali" ->
            handle_set_alias(AppkeyBin, ClientId, PublisherUidBin, Payload, MessageId, FromTagBin, ProtocolVersion, State);
        ",yaliget" ->
            get_alias(AppkeyBin, PublisherUidBin, Topic, MessageId, FromTagBin, TTL, ProtocolVersion, State);
        "MQTT_LAST_WILL" ->
            logic_MQTT_LAST_WILL:start_eredis_pool(),
            logic_MQTT_LAST_WILL:save_command(AppkeyBin, PublisherUidBin, Payload),
%% TODO:refine send_puback
            send_puback(PublisherUidBin, MessageId, FromTagBin, ProtocolVersion),
            ok;
        _ ->
            case logic_util:clientid_to_appid(ClientId) of
                {ok, Appid} ->
                    case logic_yam:check_yam_access(AppkeyBin, PublisherUidBin, Topic, ?W_TYPE, State#state.enable_yam) of
                        true ->
                            case lists:prefix(",yta", Topic) of
                                true ->
                                    publish_to_alias(AppkeyBin, PublisherUidBin, Appid, MessageId, QoS, Topic, Platform, Payload, Opts, FromTagBin, ProtocolVersion, State);
                                _ ->
                                    handle_publish_for_qos(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State)
                            end;
                        false ->
                            %%TODO:
                            ?DEBUG_MSG("no permission to publish~n"),
                            ignore
                        end;
                Else ->
                    logic_trace:trace(ClientId, undefined, undefined, <<"get appid from clientid failed">>),
                    ?ERROR("get appid from clientid failed ~p", [Else])
            end
    end.

handle_topic_filter_publish_for_qos(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State) ->
    case State#state.enable_topic_filter of
        true ->
            Timeout = State#state.topic_filter_timeout,
            case etopic_filter_client:query(logic_util:make_sure_binary(AppkeyBin), logic_util:make_sure_binary(Topic), Timeout) of
                {ok, Filters} ->
                    ?DEBUG("query topic filters return ~p", [Filters]),
                    lists:map(fun(Filter) ->
                        do_handle_publish_for_qos(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Filter, Payload, Opts, FromTagBin, ProtocolVersion, State)
                              end, Filters);
                Error ->
                    ?ERROR("query topic filters failed ~p", [Error])
            end;
        _ ->
            ignore
    end.

handle_publish_for_qos(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State = #state{
    ttl = TTL, async_publish = ASYNC_PUBLISH}) ->
    Result = do_handle_publish_for_qos(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State),
    handle_topic_filter_publish_for_qos(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State),
    Result.

do_handle_publish_for_qos(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State=#state{
    ttl=TTL, async_publish = ASYNC_PUBLISH}) ->
    StatPublishItem = State#state.stat_publish_item,
    StatPublishTime = State#state.stat_publish_time,
    logic_storage:stat_publish_count(AppkeyBin,Topic, QoS, TTL, StatPublishItem, StatPublishTime),
    case QoS of
        ?QOS_0 ->
            folsom_metrics:notify({mqtt_publish_qos0_recv, {inc, 1}}),
            case lists:prefix("$$", Topic) of
                false ->
                    case ASYNC_PUBLISH of
                        true ->
                            async_publish(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State, true);
                        _ ->
                            do_publish(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State)
                    end,

                    do_publish_to_ios_by_topic(AppkeyBin, Topic, Payload, Opts, State#state.etopicfs_get_url);
                true ->
                    ignore  %% report publish ignore
            end;
        ?QOS_1 ->
            case logic_storage:save_message_body(AppkeyBin, PublisherUidBin, Appid, MessageId, Topic, Payload, QoS, Opts, TTL) of
                ok ->
                    case ASYNC_PUBLISH of
                        true ->
                            async_publish(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State, true);
                        _ ->
                            do_publish(AppkeyBin, Appid, PublisherUidBin, MessageId, QoS, Topic, Payload, Opts, FromTagBin, ProtocolVersion, State)
                    end,
                    do_publish_to_ios_by_topic(AppkeyBin, Topic, Payload, Opts, State#state.etopicfs_get_url);
                Else ->
                    logic_trace:trace(undefined, undefined, Topic, <<"save message body error">>),
                    ?ERROR("save message body error ~p,  ~p, ~p", [Topic, MessageId, Else])
            end;
        ?QOS_2 ->
            case logic_storage:save_message_body(AppkeyBin, PublisherUidBin, Appid, MessageId, Topic, Payload, QoS, Opts, TTL) of
                ok ->
                    send_pubrec(PublisherUidBin, MessageId, FromTagBin, ProtocolVersion);
                Else ->
                    logic_trace:trace(undefined, undefined, Topic, <<"qos 2 save message body error">>),
                    ?ERROR("publish qos 2 save message body error ~p,  ~p, ~p", [Topic, MessageId, Else])
            end;
        Else ->
            ElseBin = logic_util:make_sure_binary(Else),
            logic_trace:trace(undefined, undefined, Topic, <<"qos ", ElseBin/binary  ," publish is not handled yet">>),
            ?ERROR("qos ~p publish is not handled yet", [Else]),
            ignore
    end.


%%%===================================================================
%%%  alias operation
%%%===================================================================

get_alias(AppkeyBin, PublisherUidBin, Topic, MessageId, TagBin, TTL, ProtocolVersion, State) ->
    send_puback(PublisherUidBin, MessageId, TagBin, ProtocolVersion),
    case logic_storage:get_alias_from_cb(PublisherUidBin) of
        {ok, Alias} ->
            send_publish_command_to_one_uid(AppkeyBin, TagBin, PublisherUidBin, MessageId, Topic, list_to_binary(Alias), ?QOS_1, TTL, ProtocolVersion, State);
        {error,key_enoent} ->
            ?ERROR("No Alias find for Uid ~p, MessageId ~p", [PublisherUidBin, MessageId]),
            send_publish_command_to_one_uid(AppkeyBin, TagBin, PublisherUidBin, MessageId, Topic, <<"">>, ?QOS_1, TTL, ProtocolVersion, State);
        _Else ->
            ?ERROR("Get Alias failed for Uid ~p, MessageId ~p,  ~p", [PublisherUidBin, MessageId, _Else])
    end.

% unset alias when AliasBin is empty
set_alias(AppkeyBin, PublisherUidBin, <<"">>, MessageId, FromTagBin, ProtocolVersion) ->
    case logic_storage:unset_alias(AppkeyBin, PublisherUidBin) of
        ok ->
            send_puback(PublisherUidBin, MessageId, FromTagBin, ProtocolVersion);
        Else ->
            Else
    end;
set_alias(AppkeyBin, PublisherUidBin, AliasBin, MessageId, FromTagBin, ProtocolVersion) ->
    case logic_storage:cleanup_old_uid_alias(PublisherUidBin, AppkeyBin, AliasBin) of
        ok ->
            case logic_storage:set_new_alias(AppkeyBin, PublisherUidBin, AliasBin) of
                ok ->
                    send_puback(PublisherUidBin, MessageId, FromTagBin, ProtocolVersion);
                Else ->
                    Else
            end;
        Other ->
            Other
    end.

save_alias_offline_message(AppkeyBin, PublisherUidBin, AliasBin, Appid, MessageId,
                           Topic, Payload, QoS, Opts, _State=#state{ttl=TTL, msg_queue_ttl = MSG_QUEUE_TTL,
                           msg_queue_version = MSGQ_VERSION, max_message_queue_len = MaxMsgQueueLen}) ->
    MessageIdBin = logic_util:make_sure_binary(MessageId),
    case logic_storage:save_message_body(AppkeyBin, PublisherUidBin, Appid, MessageId, Topic, Payload, QoS, Opts, TTL) of
        ok ->
            logic_storage:save_alias_offline_message_id(AppkeyBin, AliasBin, MessageIdBin, MSGQ_VERSION, MSG_QUEUE_TTL, MaxMsgQueueLen);
        Else ->
            logic_trace:trace(undefined, undefined, Topic, <<"save message body error">>),
            ?ERROR("save message body error ~p,  ~p, ~p", [Topic, MessageId, Else]),
            Else
    end.

save_offline_message(AppkeyBin, PublisherUidBin, UidBin, AliasBin, Appid, MessageId,
                     Topic, Payload, QoS, Opts, #state{ttl=TTL, msg_queue_ttl = MSG_QUEUE_TTL,
                     msg_queue_version = MSGQ_VERSION, enable_alias_offline_msg = EnableAliasOfflineMsg, max_message_queue_len = MaxMsgQueueLen}) ->
    MessageIdBin = logic_util:make_sure_binary(MessageId),
    case logic_storage:save_message_body(AppkeyBin, PublisherUidBin, Appid, MessageId, Topic, Payload, QoS, Opts, TTL) of
        ok ->
            save_offline_message_id(AppkeyBin, UidBin, AliasBin, MessageIdBin, MSGQ_VERSION, MSG_QUEUE_TTL, EnableAliasOfflineMsg, MaxMsgQueueLen);
        Else ->
            logic_trace:trace(undefined, undefined, Topic, <<"save message body error">>),
            ?ERROR("save message body error ~p,  ~p, ~p", [Topic, MessageId, Else]),
            Else
    end.

save_offline_message_id(AppkeyBin, UidBin, AliasBin, MessageIdBin, MSGQ_VERSION, MSG_QUEUE_TTL, EnableAliasOfflineMsg, MaxLen) ->
    case EnableAliasOfflineMsg of
        true ->
            %% FIXME:  save message id for uid again?
            logic_storage:save_alias_offline_message_id(AppkeyBin, AliasBin, MessageIdBin, MSGQ_VERSION, MSG_QUEUE_TTL, MaxLen);
        _ ->
            logic_storage:save_offline_message_id_to_queue(UidBin, MessageIdBin, MSGQ_VERSION, MSG_QUEUE_TTL, MaxLen, AppkeyBin)
    end.

publish_to_alias(AppkeyBin, PublisherUidBin, Appid, MessageId, QoS, Topic, Platform, Payload, Opts,
                FromTagBin, ProtocolVersion, State=#state{ttl=TTL, reg_url=RegUrl, reginfo_storage = ReginfoStorage})->
    Index = string:len(",yta/") + 1,
    AliasBin = list_to_binary(string:substr(Topic, Index)),
    ?DEBUG("publish to alias ~p, Appkey ~p, MessageId ~p, Qos ~p, Payload ~p", [AliasBin, AppkeyBin, MessageId, QoS, Payload]),

    AliasKeyBin = <<AppkeyBin/binary, "-", AliasBin/binary>>,
    case logic_storage:get_uid_by_alias(AliasKeyBin) of
        {ok, UidBin} ->
            ?DEBUG("Get Uid ~p by Alias ~p", [UidBin, AliasKeyBin]),
            Result = case save_offline_message(AppkeyBin, PublisherUidBin, UidBin, AliasBin, Appid, MessageId,
                Topic, Payload, QoS, Opts, State) of
                         {error, Reason} ->
                             logic_trace:trace(undefined, UidBin, undefined, <<"push messageId queue failed">>),
                             ?ERROR("push messageId ~p to queue faield, Uid ~p,  ~p", [MessageId, UidBin, Reason]),
                             Reason;
                         _ ->
                             ?DEBUG("push messageId ~p to queue succeed, Uid ~p", [MessageId, UidBin]),
                             alias_publish_to_one_uid(AppkeyBin, UidBin, AliasBin, QoS, MessageId, Payload,
                                 Opts, PublisherUidBin, FromTagBin, TTL, ProtocolVersion, State)
                     end,
            case logic_util:get_platform(Platform, UidBin, RegUrl, ReginfoStorage) of
                {ok, ?IOS_PLATFORM} ->
                    send_apns_by_uid(UidBin, Payload, Opts);
                _ ->
                    ignore
            end,
            Result;
        {error, key_enoent} -> %% won't save offline alias publish
            ?DEBUG("no Uid find by Alias ~p", [AliasKeyBin]),
            save_alias_offline_message(AppkeyBin, PublisherUidBin, AliasBin, Appid, MessageId, Topic, Payload, QoS, Opts, State),
            send_puback_or_publish2_ack(PublisherUidBin, MessageId, QoS, FromTagBin, Opts, ProtocolVersion);
        Else -> %%  won't save offline alias publish
            ?ERROR("find uid by alias ~p failed, ~p", [AliasKeyBin, Else])
    end.

alias_publish_to_one_uid(AppkeyBin, UidBin, AliasBin, QoS, MessageId, Payload, Opts, PublisherUidBin,
    FromTagBin, TTL, ProtocolVersion, State)->
    Topic = binary_to_list(<<",yta/", AliasBin/binary>>),
    StatPublishItem = State#state.stat_publish_item,
    StatPublishTime = State#state.stat_publish_time,
    Result = case logic_storage:get_route_table_item(UidBin, State#state.route_table_storage) of
               {ok, {Flag, _Ip, _Port, Tag}} ->
                   ?DEBUG("GET route ~p ~p", [Flag, UidBin]),
                   case Flag of
                       ?ONLINE ->
                           send_publish_command_to_one_uid(AppkeyBin, Tag, UidBin, MessageId,
                               binary_to_list(AliasBin), Payload, QoS, TTL, ProtocolVersion, State);
                        _ ->
                           ignore
                   end,
                   send_puback_or_publish2_ack(PublisherUidBin, MessageId, QoS, FromTagBin, Opts, ProtocolVersion);
               Else ->
                   logic_trace:trace(undefined, UidBin, undefined, <<"Get reoute table item failed">>),
                   ?DEBUG("Get route table item failed ~p", [Else]),
                   ignore
    end,
    logic_storage:stat_publish_count(AppkeyBin, Topic, QoS, TTL, StatPublishItem, StatPublishTime),
    Result.

handle_alias_offline_message(AppkeyBin, AliasBin, Appid, Uid, FromTag, ProtocolVersion, State) ->
    MSGQ_VERSION = State#state.msg_queue_version,
    case logic_storage:get_alias_offline_message_list(AppkeyBin, AliasBin, MSGQ_VERSION) of
        {ok, MessageIds} ->
            lists:map(fun(MessageId) ->
                MessageIdBin = logic_util:make_sure_binary(MessageId),
                case logic_publish:send_message_to_user_by_mid(AppkeyBin, Appid, Uid, MessageIdBin, FromTag, ProtocolVersion, State) of
                    {error, key_enoent} ->
                        logic_storage:remove_alias_offline_message_id(AppkeyBin, AliasBin, MessageIdBin, MSGQ_VERSION);
                    _ ->
                        ignore
                end
            end, MessageIds);
        Error ->
            ?ERROR("get alias offline message list failed ~p ~p ~p", [AppkeyBin, AliasBin, Error]),
            Error
    end.
