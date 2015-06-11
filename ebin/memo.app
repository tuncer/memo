{application, memo,
 [
  {description, "memoization server"},
  {vsn, "1.0"},
  {modules,
   [
    memo,
    memo_app,
    memo_sup
   ]},
  {applications,
   [
    kernel,
    stdlib
   ]},
  {registered, [memo]},
  {mod, {memo_app, []}},
  {env, []}
 ]}.
