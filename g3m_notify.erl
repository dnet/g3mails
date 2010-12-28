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

-module(g3m_notify).
-export([notifier/3, setup_notify/1, setup_notify/2, setup_notify/3,
	ircmain/2, ircmain/3, ircmain/4, ircproc/3]).

-include("atomizer.hrl").

% get mail every Interval ms, send message if necessary, quit if msg recv'd
notifier(Group, Interval, Pid) when is_list(Interval) ->
	notifier(Group, list_to_integer(Interval), Pid);
notifier(Group, Interval, Pid) ->
	spawn(fun() ->
		case getmails:getmails(Group) of
			[] -> nop;
			Mails -> Pid ! {g3m_notify, Mails}
		end
	end),
	receive
		_ -> quit
		after Interval -> notifier(Group, Interval, Pid)
	end.

% sets up a notifier process and returns a pid, through which it can be stopped
setup_notify(Group) -> setup_notify(Group, 90000).
setup_notify(Group, Interval) -> setup_notify(Group, Interval, self()).
setup_notify(Group, Interval, Pid) ->
	spawn(?MODULE, notifier, [Group, Interval, Pid]).

% bridges notifier to dnet's fork of erlang-ircbot
ircmain(Bot, Group) -> ircmain(Bot, Group, 90000).
ircmain(Bot, Group, Interval) ->
	ircmain(Bot, Group, Interval, "[groups] ~s sent ~s").
ircmain(Bot, Group, Interval, Format) ->
	IP = spawn(?MODULE, ircproc, [Bot, Format, null]),
	N = setup_notify(Group, Interval, IP),
	IP ! {npid, N},
	IP.

% relays messages in the appropriate form
ircproc(Bot, Format, Notify) ->
	receive
		{npid, N} when is_pid(N) -> ircproc(Bot, Format, N);
		quit when is_pid(Notify) -> Notify ! quit;
		{ident, Pid} ->
			Pid ! {ident, "g3m_notify (notifier: " ++ pid_to_list(Notify) ++ ")"},
			ircproc(Bot, Format, Notify);
		{g3m_notify, Mails} ->
			lists:foreach(
				fun(E) -> Bot ! {announce, mailfmt(E, Format)} end, Mails),
			ircproc(Bot, Format, Notify)
	end.

% formats a mail header (order: from, subject)
mailfmt(Mail, Format) ->
	lists:flatten(io_lib:format(Format,
		[Mail#feedentry.author, Mail#feedentry.title])).
