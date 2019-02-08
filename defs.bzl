def cc_lib(**kwargs):
  return native.cc_library(alwayslink = True, **kwargs)
