-module(grisp_updater_grisp2).

-behaviour(grisp_updater_system).
-behaviour(grisp_updater_http).


%--- Includes ------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").
-include_lib("grisp_updater/include/grisp_updater.hrl").


%--- Exports -------------------------------------------------------------------

% Behaviour grisp_updater_source callbacks
-export([system_init/1]).
-export([system_get_global_target/1]).
-export([system_get_active/1]).
% -export([system_get_updatable/1]).
-export([system_prepare_update/2]).
-export([system_prepare_target/4]).
-export([system_set_updated/2]).
-export([system_validate/1]).
-export([system_terminate/2]).

% Behaviour grisp_updater_http callbacks
-export([http_init/1]).
-export([http_connection_options/2]).


%--- Records -------------------------------------------------------------------

-record(sys_state, {
    current :: grisp_updater_system:system_id() | removable,
    active :: non_neg_integer(),
    update :: non_neg_integer()
}).

-record(http_state, {
    ltrim_regex :: re:mp(),
    rtrim_regex :: re:mp()
}).


%--- Behaviour grisp_updater_source Callback -----------------------------------

system_init(_Opts) ->
    ?LOG_INFO("Initializing GRiSP2 update system interface", []),
    % TODO: Uses current working directory to figure out the current
    % booted system, should be changed to use the device tree.
    #{bootstate := #{active_system := Active, update_system := Update}} =
        grisp_barebox:get_all(),
    Current = case file:get_cwd() of
        {ok, "/media/mmcsd-0-0"} -> 0;
        {ok, "/media/mmcsd-0-1"} -> 1;
        {ok, "/media/mmcsd-1-" ++ _} -> removable
    end,
    {ok, #sys_state{current = Current, active = Active, update = Update}}.

system_get_global_target(_State) ->
    #target{device = <<"/dev/mmcsd-0">>, offset = 0, size = undefined}.

system_get_active(#sys_state{current = Curr, active = Act}) ->
    {Curr, Curr =:= Act}.

% system_get_updatable(#state{}) ->
%     {error, not_updatable}.

system_prepare_update(#sys_state{current = 0} = State, 1) ->
    grisp_rtems:unmount(<<"/media/mmcsd-0-1">>),
    {ok, State};
system_prepare_update(#sys_state{current = 1} = State, 0) ->
    grisp_rtems:unmount(<<"/media/mmcsd-0-0">>),
    {ok, State};
system_prepare_update(#sys_state{current = SysId}, SysId) ->
    {error, cannot_update_running_system};
system_prepare_update(_State, SysId) ->
    {error, {invalid_update_system, SysId}}.

system_prepare_target(_State, _SysId,
                      #target{offset = SysOffset, size = SysSize} = SysTarget,
                      #raw_target_spec{context = system, offset = ObjOffset})
  when ObjOffset >= 0, ObjOffset < SysSize ->
    {ok, SysTarget#target{
        offset = SysOffset + ObjOffset,
        size = SysSize - ObjOffset
    }}.

system_set_updated(#sys_state{current = 0} = State, 1) ->
    grisp_barebox:set([bootstate, update_system], 1),
    grisp_barebox:set([bootstate, update_boot_count], 1),
    grisp_barebox:commit(),
    {ok, State};
system_set_updated(#sys_state{current = 1} = State, 0) ->
    grisp_barebox:set([bootstate, update_system], 0),
    grisp_barebox:set([bootstate, update_boot_count], 1),
    grisp_barebox:commit(),
    {ok, State}.

system_validate(#sys_state{current = SysId, active = SysId} = State) ->
    {ok, State};
system_validate(#sys_state{current = SysId, update = SysId} = State) ->
    grisp_barebox:set([bootstate, active_system], SysId),
    grisp_barebox:set([bootstate, update_boot_count], 0),
    grisp_barebox:commit(),
    {ok, State#sys_state{active = SysId}};
system_validate(#sys_state{current = removable}) ->
    {error, current_system_removable}.

system_terminate(_State, _Reason) ->
    ?LOG_INFO("Terminating GRiSP update system interface", []),
    ok.


%--- Behaviour grisp_updater_http Callback -------------------------------------

http_init(_Opts) ->
    {ok, #http_state{}}.

http_connection_options(State, Url) ->
    case uri_string:parse(Url) of
        #{scheme := <<"https">>, host := Host} = Parts ->
            Hostname = unicode:characters_to_list(Host),
            Port = maps:get(port, Parts, 443),
            Opts = tls_options(Host),
            {ok, Hostname, Port, Opts, State};
        #{scheme := <<"http">>, host := Host} = Parts ->
            Hostname = unicode:characters_to_list(Host),
            Port = maps:get(port, Parts, 80),
            {ok, Hostname, Port, #{}, State};
        _ ->
            not_supported
    end.


%--- Internal Functions --------------------------------------------------------

tls_options(Host) ->
    Priv = code:priv_dir(grisp_cryptoauth),
    {ok, CLICAPEM1} = file:read_file(filename:join([Priv, "grisp2_ca.pem"])),
    {ok, CLICAPEM2} = file:read_file(filename:join([Priv, "stritzinger_root.pem"])),
    [{'Certificate', CLICADER1, not_encrypted}] = public_key:pem_decode(CLICAPEM1),
    [{'Certificate', CLICADER2, not_encrypted}] = public_key:pem_decode(CLICAPEM2),
    TransportOpts = [
        {server_name_indication, unicode:characters_to_list(Host)},
        {cacerts, [CLICADER2]},
        {cert, [grisp_cryptoauth:read_cert(primary, der), CLICADER1]},
        {key, #{algorithm => ecdsa, sign_fun => {grisp_cryptoauth, sign_fun}}}
    ],
    #{transport => tls, transport_opts => TransportOpts}.
