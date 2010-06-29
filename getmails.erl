% Copyright (c) 2010 András Veres-Szentkirályi
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

-compile(export_all).
-module(getmails).

-include("atomizer.hrl").

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
