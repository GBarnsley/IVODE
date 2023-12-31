library(devtools)

odin::odin_package(here::here())
document()
load_all()

use_r("format")
use_test("format")
use_package("methods")

test()
check()
install()

build_readme()
