library(devtools)

odin::odin_package(here::here())
document()
load_all()

use_r("project")
use_test("project")
use_package("methods")

test()
check()
install()

build_readme()
