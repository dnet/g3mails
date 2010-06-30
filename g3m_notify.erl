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
-compile(export_all). % not for debug, _every_ function needs to be exported

% get mail every Interval ms, send message if necessary, quit if msg recv'd
notifier(Group, Interval, Pid) ->
	case getmails:getmails(Group) of
		[] -> nop;
		Mails -> Pid ! {g3m_notify, Mails}
	end,
	receive
		_ -> quit
		after Interval -> notifier(Group, Interval, Pid)
	end.

% sets up a notifier process and returns a pid, through which it can be stopped
setup_notify(Group) -> setup_notify(Group, 90000).
setup_notify(Group, Interval) -> setup_notify(Group, Interval, self()).
setup_notify(Group, Interval, Pid) ->
	spawn(?MODULE, notifier, [Group, Interval, Pid]).
