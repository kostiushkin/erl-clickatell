{application, clickatell,
 [{description, "Clickatell SMS server"},
  {vsn, "0.1"},
  {modules, [clickatell_app, clickatell_sup, clickatell]},
  {registered, [clickatell]},
  {applications, [kernel, stdlib]},
  {mod, {clickatell_app, []}},
  {env, []}
 ]}.
