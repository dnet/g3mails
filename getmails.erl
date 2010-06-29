-compile(export_all).
-module(getmails).

-include("../src/atomizer.hrl").

% generate already-seen file name
alse_fn(Group) -> "getmails-alse-" ++ Group ++ ".txt".

% write entries that were already seen to file
write_alse(Entries, Group) ->
	{ok, FD} = file:open(alse_fn(Group), [write]),
	lists:foreach(fun(Entry) ->
		io:format(FD, "~s~n", [Entry#feedentry.permalink]) end, Entries),
	file:close(FD).

% read entries that were already seen from file
read_alse(Group) ->
	{ok, FD} = file:open(alse_fn(Group), [read]),
	read_alse(FD, []).

% read entries that were already seen from file descriptor
read_alse(FD, List) ->
	case io:get_line(FD, "") of
		eof -> file:close(FD), List;
		Line -> read_alse(FD, [string:strip(Line, right, $\n) | List])
	end.

getmails() -> getmails("hspbp").
getmails(Group) ->
	Feed = atomizer:parse_url(
		"http://groups.google.com/group/" ++ Group ++ "/feed/atom_v1_0_msgs.xml"),
	AlSe = read_alse(Group),
	Entries = Feed#feed.entries,
	write_alse(Entries, Group),
	News = lists:filter(fun(Entry) -> not lists:member(Entry#feedentry.permalink, AlSe) end, Entries).
