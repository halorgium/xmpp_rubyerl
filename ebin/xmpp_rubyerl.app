{application, xmpp_rubyerl,
 [{description, "XMPP process"},
  {vsn, "0.1"},
  {applications, [kernel, stdlib, sasl]},
  {mod, {xmpp_rubyerl, []}},
  {modules, [xmpp_rubyerl, xmpp_notifier_sup, xmpp_notifier]},
  {registered, [xmpp_notifier_sup]}
 ]}.
