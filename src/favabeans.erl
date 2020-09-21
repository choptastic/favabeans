%%%-------------------------------------------------------------------
%%% @author Jesse Gumm
%%% @copyright (C) 2020, Jesse Gumm
%%% @doc
%%%
%%% @end
%%% Created : 2020-09-21 13:02:04.795831
%%%-------------------------------------------------------------------
-module(favabeans).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([pause/0]).

-define(SERVER, ?MODULE).

-record(state, {
    filename="erlang_processes",
    suffix=1,
    status=idle,
    gatherer,
    delay=1000
    }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

pause() ->
    gen_server:cast(?SERVER, pause).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Opts) ->
    Delay = proplists:get_value(delay, Opts, 1000),
    timer:send_after(Delay, self(), gather),
    {ok, #state{delay=Delay}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(status, _From, State) ->
        Reply = {State#state.status, State#state.gatherer},
        {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(gather_complete, State=#state{suffix=Suffix, delay=Delay}) ->
    NewSuffix = (Suffix rem 5) + 1,
    NewState = State#state{suffix=NewSuffix, status=idle, gatherer=undefined},
    timer:send_after(Delay, self(), gather),
    {noreply, NewState}.

gather(Filename, Suffix) ->
    FinalFilename = Filename ++ "." ++ wf:to_list(Suffix),
    {ok, File} = file:open(FinalFilename, write),
    Pids = erlang:processes(),
    lists:foreach(fun(Pid) ->
        Memory = case erlang:process_info(Pid, memory) of
            {memory, M} -> {memory, M};
            _-> {memory, 0}
        end,
        Info = erlang:process_info(Pid),
        Data = {Pid, [Memory | Info]},
        io:format(File, "~p.~n~n",[Data])
    end, Pids),
    file:close(File),
    gen_server:cast(?SERVER, gather_complete).


%sort_by(Pids, memory) ->
%    WithMem = add_memory(Pids),
%    Sorted = lists:sort(fun({A, MemA}, {B, MemB}) ->
%        MemA >= MemB
%    end, WithMem),
%    [Pid || {Pid,_} <- Sorted].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(gather, State) ->
    NewState = case State#state.status of
        gathering ->
            State;
        idle ->
            Pid = erlang:spawn(fun() ->
                gather(State#state.filename, State#state.suffix) 
            end),
            erlang:monitor(process, Pid),
            State#state{status=gathering, gatherer=Pid}
    end,
    {noreply, NewState};
handle_info(_, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




