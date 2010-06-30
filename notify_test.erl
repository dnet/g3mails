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

-module(notify_test).
-export([test/1, test/2]).

% print received mails in a loop until receiving an nt_stop message
printer() ->
	receive
		nt_stop -> quit; 
		{g3m_notify, Msg} -> io:format("New messages: ~p~n", [Msg]), printer()
	end.

test(Group) -> test(Group, 10000). % smaller interval for testing purposes
test(Group, Interval) ->
	Printer = spawn(fun printer/0),
	Pid = g3m_notify:setup_notify(Group, Interval, Printer),
	io:get_line("Press return to quit"),
	Pid ! nt_stop,
	Printer ! nt_stop,
	ok.
