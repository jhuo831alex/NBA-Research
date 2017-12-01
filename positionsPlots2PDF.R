#Ignat Kulinka
#this code makes nice pdf's out of positions plots and short charts with matching fonts

#center
library(extrafont)
ggsave(path.expand("~/Desktop/latex paper/center.pdf"), center, scale = .7, family = "CM Roman", width=6, height=4)
embed_fonts(path.expand("~/Desktop/latex paper/center.pdf"),
            outfile=path.expand("~/Desktop/latex paper/center.pdf"))

#shooting guard
ggsave(path.expand("~/Desktop/latex paper/shootingGuard.pdf"), shootingGuard, scale = .7, family = "CM Roman", width=6, height=4)
embed_fonts(path.expand("~/Desktop/latex paper/shootingGuard.pdf"),
            outfile=path.expand("~/Desktop/latex paper/shootingGuard.pdf"))


#power forward
ggsave(path.expand("~/Desktop/latex paper/powerForward.pdf"), powerForward, scale = .7, family = "CM Roman", width=6, height=4)
embed_fonts(path.expand("~/Desktop/latex paper/powerForward.pdf"),
            outfile=path.expand("~/Desktop/latex paper/powerForward.pdf"))

#point guard
ggsave(path.expand("~/Desktop/latex paper/pointGuard.pdf"), pointGuard, scale = .7, family = "CM Roman", width=6, height=4)
embed_fonts(path.expand("~/Desktop/latex paper/pointGuard.pdf"),
            outfile=path.expand("~/Desktop/latex paper/pointGuard.pdf"))

#small forward
ggsave(path.expand("~/Desktop/latex paper/smallForward.pdf"), smallForward, scale = .7, family = "CM Roman", width=6, height=4)
embed_fonts(path.expand("~/Desktop/latex paper/smallForward.pdf"),
            outfile=path.expand("~/Desktop/latex paper/smallForward.pdf"))


#code for the shot charts 
ggsave(path.expand("~/Desktop/UCLA/Spring 2017/STATS 199/STATS 199 Research/Latex Paper/FT.pdf"), g4, family = "CM Roman", width=6, height=5)
embed_fonts(path.expand("~/Desktop/UCLA/Spring 2017/STATS 199/STATS 199 Research/Latex Paper/FT.pdf"),
            outfile=path.expand("~/Desktop/UCLA/Spring 2017/STATS 199/STATS 199 Research/Latex Paper/FT.pdf"))
