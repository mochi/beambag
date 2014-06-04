% anonymous function
% in: <type of report: error or success>, <package file>, <reason in case of error>, <extra arguments ...>
% out: n/a
fun(Type, File, Reason, Emails) ->
    {ok, Hostname} = inet:gethostname(),
    Subject = "Propadata " ++ Type ++ " on " ++ Hostname ++ " with " ++ File,
    SafeSubject = lists:map(fun($') -> $"; (O) -> O end, Subject),
    Text = [string:to_upper(hd(Type))] ++ tl(Type) ++ " installing " ++ File ++ " on " ++ Hostname ++ ": "
            ++ lists:flatten(io_lib:format("~p", [Reason])),
    SafeText = lists:map(fun($') -> $"; (O) -> O end, Text),
    os:cmd("/bin/echo '" ++ SafeText ++ "' | /usr/bin/mail -s '" ++ SafeSubject ++ "' " ++ string:join(Emails, " "))
end.
