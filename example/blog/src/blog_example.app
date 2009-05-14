{application, blog_example,
 [{description, "blog_example"},
  {vsn, "0.01"},
  {modules, [
    blog_example,
    blog_example_app,
    blog_example_sup,
    blog_example_web,
    blog_example_deps
  ]},
  {registered, []},
  {mod, {blog_example_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
