def cc_lib(name, deps, srcs = [], test_deps = [], header_only = False,
           test_data = None, no_header = False, **kwargs):
  native.cc_library(
      name = name,
      hdrs = [name + ".h"] if not no_header else [],
      srcs = [] if header_only else [name + ".cc"] + srcs,
      deps = deps,
      **kwargs
  )

  if test_deps != None:
    native.cc_test(
        name = name + "-test",
        srcs = [name + "_test.cc"],
        deps = ["//test", ":" + name] + test_deps,
        data = test_data
    )
