
.onLoad = function(libname, pkgname) {
# START: maps
.env = parent.frame(1)$nsenv

delayedAssign(".maps_board",{

tmp_path = paste0(
  c(parent.frame(1)$ns_path, "pkgdown", "assets", "maps"),
  collapse = .Platform$file.sep
)
if (dir.exists(tmp_path)) {
  tmp = pins::board_folder(tmp_path)
} else {
  tmp = pins::board_url(
    "https://terminological.github.io/arear/maps"
  )
  }
tmp
}, assign.env = .env)
delayedAssign("CA19", pins::pin_read(.maps_board,"CA19"), assign.env = .env)
delayedAssign("CCG20", pins::pin_read(.maps_board,"CCG20"), assign.env = .env)
delayedAssign("CTRY19", pins::pin_read(.maps_board,"CTRY19"), assign.env = .env)
delayedAssign("CTYUA19", pins::pin_read(.maps_board,"CTYUA19"), assign.env = .env)
delayedAssign("DZ11", pins::pin_read(.maps_board,"DZ11"), assign.env = .env)
delayedAssign("GBR_ISO3166_2", pins::pin_read(.maps_board,"GBR_ISO3166_2"), assign.env = .env)
delayedAssign("GBR_ISO3166_3", pins::pin_read(.maps_board,"GBR_ISO3166_3"), assign.env = .env)
delayedAssign("GOOGLE_MOBILITY", pins::pin_read(.maps_board,"GOOGLE_MOBILITY"), assign.env = .env)
delayedAssign("HB19", pins::pin_read(.maps_board,"HB19"), assign.env = .env)
delayedAssign("LAD19", pins::pin_read(.maps_board,"LAD19"), assign.env = .env)
delayedAssign("LAD20", pins::pin_read(.maps_board,"LAD20"), assign.env = .env)
delayedAssign("LGD12", pins::pin_read(.maps_board,"LGD12"), assign.env = .env)
delayedAssign("LHB19", pins::pin_read(.maps_board,"LHB19"), assign.env = .env)
delayedAssign("LSOA11", pins::pin_read(.maps_board,"LSOA11"), assign.env = .env)
delayedAssign("MSOA11", pins::pin_read(.maps_board,"MSOA11"), assign.env = .env)
delayedAssign("NHSER20", pins::pin_read(.maps_board,"NHSER20"), assign.env = .env)
delayedAssign("OUTCODE", pins::pin_read(.maps_board,"OUTCODE"), assign.env = .env)
delayedAssign("PHEC16", pins::pin_read(.maps_board,"PHEC16"), assign.env = .env)
delayedAssign("WD11", pins::pin_read(.maps_board,"WD11"), assign.env = .env)
delayedAssign("WD19", pins::pin_read(.maps_board,"WD19"), assign.env = .env)
# END: %s
}
