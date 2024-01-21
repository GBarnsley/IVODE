library(devtools)

odin::odin_package(here::here())
#odin.dust::odin_dust_package(here::here())
document()
load_all()

use_r("sample_parameters")
use_test("sample_parameters")
use_package("methods")

test()
check()
install()

build_readme()
