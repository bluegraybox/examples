%% cribbed from "Programming Erlang" p. 352
{application, math,
 [{description, "Simple math the hard way"},
  {vsn, "1.0"},
  {modules, [math_app, math_supervisor, math_server]},
  {registered, [math_server]},
  {applications, [kernel, stdlib]},
  {mod, {math_app, []}},
  {start_phases, []}
 ]}.

