-module(rebar_rsync_resource).

-export([init/2,
         lock/2,
         download/4, 
         %download/3,
         needs_update/2,
         make_vsn/2]).

-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).
-define(CRASHDUMP(Str, Args), rebar_log:crashdump(Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).


%% Initialize the custom dep resource plugin
init(Type, _RebarState) ->
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
    {ok, Resource}.

lock(AppInfo, _) ->
    check_type_support(),
    lock_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

lock_(AppDir, {rsync, Url, _}) ->
    lock_(AppDir, {rsync, Url});

lock_(AppDir, {rsync, Url}) ->
    Ref = get_ref(AppDir),
    {rsync, Url, {ref, Ref}}.

get_ref(Dir) ->
    AbortMsg = lists:flatten(io_lib:format("Locking of rsync dependency failed in ~ts", [Dir])),
    Dir2 = rebar_utils:escape_double_quotes(Dir),
    {ok, VsnString} =
                rebar_utils:sh("find \"" ++ Dir2 ++ "\" -type f  -exec md5sum  {} \; | sort | md5sum",
                                                [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),

    Ref = rebar_string:trim(VsnString, both, "\n -"),
    Ref.


needs_update(AppInfo, _) ->
    check_type_support(),
    needs_update_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).


needs_update_(_Dir, {rsync, _Url, "master"}) -> true;
% we do not separate What: tag, branch,
needs_update_(Dir, {rsync, _Url, {_What, Tag}}) ->
    Current = get_ref(Dir),
    ?DEBUG("Comparing git tag ~ts with ~ts", [Tag, Current]),
    not (Current =:= Tag).


download(TmpDir, AppInfo, State, _) ->
    check_type_support(),
    case download_(TmpDir, rebar_app_info:source(AppInfo), State) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason};
        Error ->
            {error, Error}
    end.


download_(Dir, {rsync, Url, _Tag}, _State) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("rsync -az --delete ~s/ ~s", [Url, Dir]), []).


make_vsn(_Dir, _ResourceState) ->
  % for the rsync version is not relevant.
  % return error..
  %{plain, "0.0.1"}.
  {error,"Version not supported for rsync"}.



%% --------- apr ----------------

check_type_support() ->
    case get({is_supported, ?MODULE}) of
        true ->
            ok;
        _ ->
            case rebar_utils:sh("rsync --version", [{return_on_error, true},
                                                  {use_stdout, false}]) of
                {error, _} ->
                    ?ABORT("rsync not installed", []);
                _ ->
                    put({is_supported, ?MODULE}, true),
                    ok
            end
    end.

