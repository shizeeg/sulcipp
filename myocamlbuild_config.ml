type c_binding = {
  name: string;
  path: string;
  include_dir: string;
  lib_dir: string;
  lib: string;
  headers: string list
}
    
let lua_binding = {
  name = "lua";
  path = "lib/mllua";
  include_dir = "-I/usr/local/include/lua51";
  lib_dir = "-L/usr/local/lib";
  lib = "-llua-5.1";
  headers = []
}

let resolv_binding = {
  name = "resolv";
  path = "lib/mlresolv";
  include_dir = "";
  lib_dir = "";
  lib = "-lc";
  headers = []
}
  
let tls_binding = {
  name = "tls";
  path = "lib/mltls";
  include_dir = "";
  lib_dir = "";
  lib = "-lssl";
  headers = []
}

let zlib_binding = {
  name = "zlib";
  path = "lib/mlzlib";
  include_dir = "";
  lib_dir = "";
  lib = "-lz";
  headers = []
}

let zlib_binding = {
  name = "zlib";
  path = "lib/mlzlib";
  include_dir = "";
  lib_dir = "";
  lib = "-lz";
  headers = []
}

let static = true

let enable_lua = true
let enable_tls = true
let enable_resolv = true
let enable_zlib = true
