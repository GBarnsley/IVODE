library(devtools)

load_all()

use_r("checks")
use_test("run_model")
use_package()

document()
test()
check()
install()

odin::odin_package(here::here())

build_readme()
