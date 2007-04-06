`display.alts` <-
function()
{
     # Function to display commonly used ALT sequences on a PC keyboard.
     #
     frame()
     oldpar <- par()
     on.exit(par(oldpar))
     par(usr = c(0, 5, 0, 6))
     # anum uses PC-850 Multilingual decimals for use with Alt
     anum <- c(143, 144, 130, 138, 136, 205, 135, 148, 155, 225, 159, 248, 251,
          253, 252, 246, 158,241, 174, 175, 172, 171, 243, 169, 184, 230)
     # achr uses Windows 3.1 Latin 1 octal codes
     achr <- c("\305", "\311", "\351", "\350", "\352", "\340", "\347", "\366", "\370",
         "\337", "\203","\260", "\271", "\262", "\263", "\367", "\327", "\261", "\253",
         "\273", "\274", "\275","\276", "\256", "\251", "\265", "\261")
     mtext("Table of Alt codes (PC-850 Multilingual)", side = 1, line = 0.7, cex = 1.5)
     nindx <- length(anum) - 1
     for(i in 0:nindx) {
         x <- 0.5 + (i %% 5)
         y <- 6 - (0.5 + (i %/% 5))
         text(x + 0.2, y - 0.2, achr[i + 1], cex = 2)
         text(x - 0.2, y + 0.2, anum[i + 1], adj = 0.5, cex = 1.5)
     }
     abline(h = 0:6, lty = 1)
     abline(v = 0:5, lty = 1)
     invisible()
}

