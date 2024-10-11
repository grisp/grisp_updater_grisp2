-module(grisp_updater_grisp2).

-behaviour(grisp_updater_system).
-behaviour(grisp_updater_http).


%--- Includes ------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").
-include_lib("grisp/include/grisp.hrl").
-include_lib("grisp_updater/include/grisp_updater.hrl").


%--- Exports -------------------------------------------------------------------

% Behaviour grisp_updater_source callbacks
-export([system_init/1]).
-export([system_get_global_target/1]).
-export([system_get_systems/1]).
-export([system_prepare_update/2]).
-export([system_prepare_target/4]).
-export([system_set_updated/2]).
-export([system_cancel_update/1]).
-export([system_validate/1]).
-export([system_terminate/2]).

% Behaviour grisp_updater_http callbacks
-export([http_init/1]).
-export([http_connection_options/2]).


%--- Records -------------------------------------------------------------------

-record(sys_state, {
    target :: target(),
    boot :: grisp_updater_system:system_id() | removable,
    valid :: grisp_updater_system:system_id(),
    update :: grisp_updater_system:system_id(),
    next :: grisp_updater_system:system_id()
}).

-record(http_state, {
    ltrim_regex :: re:mp(),
    rtrim_regex :: re:mp()
}).


%--- Behaviour grisp_updater_source Callback -----------------------------------

system_init(_Opts) ->
    ?LOG_INFO("Initializing GRiSP2 update system interface", []),
    {ok, update_state(#sys_state{
        target = init_target(),
        boot = boot_system()
    })}.

system_get_global_target(#sys_state{target = Target}) ->
    Target.

system_get_systems(#sys_state{boot = Boot, next = Next, valid = Valid}) ->
    {Boot, Valid, Next}.

system_prepare_update(#sys_state{valid = SysId}, SysId) ->
    {error, cannot_update_validated_system};
system_prepare_update(#sys_state{boot = SysId}, SysId) ->
    {error, cannot_update_boot_system};
system_prepare_update(#sys_state{boot = Boot, valid = 0} = State, 1)
  when Boot =:= removable; Boot =:= 0 ->
    unmount_system(1),
    {ok, State};
system_prepare_update(#sys_state{boot = Boot, valid = 1} = State, 0)
  when Boot =:= removable; Boot =:= 1 ->
    unmount_system(0),
    {ok, State};
system_prepare_update(#sys_state{boot = Boot, valid = Valid}, SysId) ->
    {error, {unexpected_update_state, {Boot, Valid, SysId}}}.

system_prepare_target(_State, _SysId,
                      #target{offset = SysOffset, size = SysSize} = SysTarget,
                      #raw_target_spec{context = system, offset = ObjOffset})
  when ObjOffset >= 0, ObjOffset < SysSize ->
    {ok, SysTarget#target{
        offset = SysOffset + ObjOffset,
        size = SysSize - ObjOffset
    }}.

system_set_updated(#sys_state{boot = Boot, valid = 0} = State, 1)
  when Boot =:= removable; Boot =:= 0 ->
    {ok, mark_updated(State, 1)};
system_set_updated(#sys_state{boot = Boot, valid = 1} = State, 0)
  when Boot =:= removable; Boot =:= 1 ->
    {ok, mark_updated(State, 0)};
system_set_updated(#sys_state{boot = Boot, valid = Valid}, SysId) ->
    {error, {unexpected_update_state, {Boot, Valid, SysId}}}.

system_cancel_update(#sys_state{valid = Valid} = State) ->
    {ok, validate_update(State, Valid)}.

system_validate(#sys_state{boot = SysId, update = SysId} = State) ->
    {ok, validate_update(State, SysId)};
system_validate(#sys_state{update = UpdatSysId}) ->
    {error, {validate_from_unbooted, UpdatSysId}}.

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
            GunOpts = #{
                transport => tls,
                tls_opts => grisp_cryptoauth_tls:options(Hostname)
            },
            {ok, Hostname, Port, GunOpts, State};
        #{scheme := <<"http">>, host := Host} = Parts ->
            Hostname = unicode:characters_to_list(Host),
            Port = maps:get(port, Parts, 80),
            {ok, Hostname, Port, #{}, State};
        _ ->
            not_supported
    end.


%--- Internal Functions --------------------------------------------------------

-ifdef(EMULATE_HARDWARE).

-define(DEVICE_FILE, <<"dummy.img">>).
-define(DEVICE_SIZE, ((4 + 256 + 256) * (1024 * 1024))).

init_target() ->
    DeviceFile = ?DEVICE_FILE,
    DeviceSize = ?DEVICE_SIZE,
    case file:open(?DEVICE_FILE, [raw, write, read]) of
        {error, Reason} -> error(Reason);
        {ok, File} ->
            case file:pread(File, DeviceSize - 1, 1) of
                {error, Reason} -> error(Reason);
                {ok, [_]} ->
                    ok = file:close(File);
                eof ->
                    ok = file:pwrite(File, DeviceSize - 1, <<0>>),
                    ok = file:close(File)
            end
    end,
    #target{device = DeviceFile, offset = 0,
            size = DeviceSize, total = DeviceSize}.

% boot_system() -> removable.
boot_system() -> 0.

unmount_system(_SysId) -> ok.

-else. % EMULATE_HARDWARE is undefined

init_target() ->
    #target{device = <<"/dev/mmcsd-0">>, offset = 0}.

boot_system() ->
    % TODO: Uses current working directory to figure out the current
    % booted system, should be changed to use the device tree.
    case file:get_cwd() of
        {ok, "/media/mmcsd-0-0"} -> 0;
        {ok, "/media/mmcsd-0-1"} -> 1;
        {ok, "/media/mmcsd-1-" ++ _} -> removable
    end.

unmount_system(0) -> grisp_rtems:unmount(<<"/media/mmcsd-0-0">>);
unmount_system(1) -> grisp_rtems:unmount(<<"/media/mmcsd-0-1">>).

-endif.

update_state(State) ->
    ActiveSysId = grisp_barebox:get([bootstate, active_system]),
    UpdateSysId = grisp_barebox:get([bootstate, update_system]),
    BootCount = grisp_barebox:get([bootstate, update_boot_count]),
    NextSysId = case {ActiveSysId, UpdateSysId, BootCount} of
        {_ActiveSysId, UpdateSysId, Count} when Count > 0 -> UpdateSysId;
        {ActiveSysId, _UpdateSysId, 0} -> ActiveSysId
    end,
    State#sys_state{
        next = NextSysId,
        update = UpdateSysId,
        valid = ActiveSysId
    }.

mark_updated(State, SysId) ->
    grisp_barebox:set([bootstate, update_system], SysId),
    grisp_barebox:set([bootstate, update_boot_count], 1),
    grisp_barebox:commit(),
    update_state(State).

validate_update(State, SysId) ->
    grisp_barebox:set([bootstate, active_system], SysId),
    grisp_barebox:set([bootstate, update_boot_count], 0),
    grisp_barebox:commit(),
    update_state(State).
