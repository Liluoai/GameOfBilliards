%%%-------------------------------------------------------------------
%%% @author Liluoai
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十一月 2016 16:12
%%%-------------------------------------------------------------------
-module(logic_MQTT_LAST_WILL).
-author("XuLei").

-compile({parse_transform, lager_transform}).

-include("mqtt_frame.hrl").

-include_lib("elog/include/elog.hrl").
-include_lib("emqtt/include/emqtt_frame.hrl").
-include_lib("msgbus_common_utils/include/internal_package_pb.hrl").

%% API
-export([start_eredis_pool/0, save_command/3]).



start_eredis_pool() ->
  Result = eredis_pool:create_pool(pool1, 10, "127.0.0.1", 6379, 1, "abc", 100),
  ?DEBUG("~p", [Result]).

save_command(AppkeyBin, PublisherUidBin, Payload) ->
  ?DEBUG("~p", [AppkeyBin]),
  ?DEBUG("~p", [PublisherUidBin]),
  ?DEBUG("~p", [Payload]),
  {Topic_of_MQTT_LAST_WILL, Payload_of_MQTT_LAST_WILL, Qos_of_MQTT_LAST_WILL, Retain_of_MQTT_LAST_WILL} = get_detail_message_of_MQTT_LAST_WILL(Payload),
  Unique_id = <<AppkeyBin/binary, PublisherUidBin/binary>>,
  eredis_pool:q(pool1, ["HSET", Unique_id, "topic", Topic_of_MQTT_LAST_WILL]),
  eredis_pool:q(pool1, ["HSET", Unique_id, "payload", Payload_of_MQTT_LAST_WILL]),
  eredis_pool:q(pool1, ["HSET", Unique_id, "qos", Qos_of_MQTT_LAST_WILL]),
  eredis_pool:q(pool1, ["HSET", Unique_id, "retain", Retain_of_MQTT_LAST_WILL]),
  ok.

get_detail_message_of_MQTT_LAST_WILL(Payload0) ->
  Detail_message_of_MQTT_LAST_WILL = binary_to_list(Payload0),
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


