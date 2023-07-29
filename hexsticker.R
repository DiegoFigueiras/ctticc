## using `hexSticker` to make a sticker - 6/26/23

library(hexSticker)
library(ctticc)
library(magick)
library(tidyverse)

ICCimage1 <- image_read("together.png")   ## package plots
ICCimage2 <- image_read("robin.png")      ## cartoon
ICCimage3 <- image_read("routerbit.PNG")  ## ogive router bit
ICCimage4 <- image_read("iccs.PNG")
ICCimage5 <- image_read("Cartoon_Penguin_clip_art.svg")

sticker(subplot=ICCimage1,
        package="ctticc",
        s_width=1.5,                    ## s = image
        s_height=1,
        s_x = 1,
        s_y = 1,
        p_size = 20,                    ## p = package name
        p_color = "white",
        p_x = 1,
        p_y = 1.7,
        h_fill = "#AA3BC2",             ## h = border
        h_color = "hotpink",
        h_size = 2.2,
        url="https://github.com/MontclairML/ctticc",
        u_size = 3,                     ## u = url
        u_color = "white",
        spotlight = T
) %>% print()


sticker(subplot=ICCimage2,
        package="ctticc2",
        s_width=1.5,                    ## s = image
        s_height=1,
        s_x = 1,
        s_y = .9,
        p_size = 20,                    ## p = package name
        p_color = "white",
        p_x = 1,
        p_y = 1.6,
        h_fill = "#AA3BC2",             ## h = border
        h_color = "hotpink",
        h_size = 2.2,
        url="https://github.com/MontclairML/ctticc",
        u_size = 3.5,                     ## u = url
        u_color = "white",
        spotlight = T
) %>% print()


sticker(subplot=ICCimage3,
        package="ctticc3",
        s_width=1.4,                    ## s = image
        s_height=.9,
        s_x = 1,
        s_y = .9,
        p_size = 20,                    ## p = package name
        p_color = "white",
        p_x = 1,
        p_y = 1.6,
        h_fill = "#AA3BC2",             ## h = border
        h_color = "hotpink",
        h_size = 2.2,
        url="https://github.com/MontclairML/ctticc",
        u_size = 3.5,                     ## u = url
        u_color = "white",
        spotlight = T
) %>% print()


sticker(subplot=ICCimage4,
        package="ctticc",
        s_width=1.2,                    ## s = image
        s_height=.8,
        s_x = 1,
        s_y = .95,
        p_size = 20,                    ## p = package name
        p_color = "black",
        p_x = 1,
        p_y = 1.6,
        h_fill = "white",             ## h = border
        h_color = "black",
        h_size = 2.2,
        url="https://github.com/MontclairML/ctticc",
        u_size = 3,                     ## u = url
        u_color = "black",
        spotlight = F
) %>% print()

sticker(subplot=ICCimage5,
        package="ctticc5",
        s_width=1.4,                    ## s = image
        s_height=1,
        s_x = 1,
        s_y = .9,
        p_size = 20,                    ## p = package name
        p_color = "black",
        p_x = 1,
        p_y = 1.6,
        h_fill = "white",             ## h = border
        h_color = "black",
        h_size = 2.2,
        url="https://github.com/MontclairML/ctticc",
        u_size = 3,                     ## u = url
        u_color = "black",
        spotlight = F
) %>% print()



