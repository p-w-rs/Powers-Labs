module Colors exposing (..)

import Element exposing (el, rgb, rgba, text, transparent)



-- AYU COLORS


c_tag =
    -- blue
    rgb (57 / 255) (186 / 255) (230 / 255)


c_func =
    -- oj
    rgb (255 / 255) (180 / 255) (84 / 255)


c_entity =
    -- blue
    rgb (89 / 255) (194 / 255) (255 / 255)


c_string =
    -- green
    rgb (170 / 255) (217 / 255) (76 / 255)


c_regexp =
    -- green
    rgb (149 / 255) (230 / 255) (203 / 255)


c_markup =
    -- red
    rgb (240 / 255) (113 / 255) (120 / 255)


c_keyword =
    -- oj
    rgb (255 / 255) (143 / 255) (79 / 255)


c_special =
    -- tan
    rgb (230 / 255) (182 / 255) (115 / 255)


c_comment =
    -- grey
    rgb (172 / 255) (182 / 255) (191 / 255)


c_constant =
    -- purple
    rgb (210 / 255) (166 / 255) (255 / 255)


c_operator =
    -- oj
    rgb (242 / 255) (150 / 255) (104 / 255)


c_added =
    -- green
    rgb (127 / 255) (217 / 255) (98 / 255)


c_modified =
    -- blue
    rgb (115 / 255) (184 / 255) (255 / 255)


c_removed =
    -- pink
    rgb (242 / 255) (109 / 255) (120 / 255)


c_fg =
    -- grey
    rgb (191 / 255) (189 / 255) (182 / 255)


c_bg =
    -- black
    rgb (13 / 255) (16 / 255) (23 / 255)


c_line =
    -- black
    rgb (19 / 255) (23 / 255) (33 / 255)


c_warn =
    -- yellow
    rgb (230 / 255) (180 / 255) (80 / 255)


c_err =
    -- red
    rgb (217 / 255) (87 / 255) (87 / 255)


inv =
    rgba 0.0 0.0 0.0 0.0



-- FILLERS


hide n =
    el [ transparent True ] (text <| String.repeat n " ")


lorem100 : String
lorem100 =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris viverra elit eget lectus congue, in elementum nisl placerat. Aliquam quis felis est. Donec id quam et nibh posuere molestie. Sed tortor ante, pulvinar non sem at, pulvinar egestas felis. Mauris volutpat quam eu risus mollis finibus nec in erat. Curabitur accumsan nec augue vel interdum. Donec et interdum magna. Pellentesque rutrum lorem dui, eget posuere augue varius ut. Maecenas ac hendrerit ipsum, nec euismod massa. Integer congue dui tincidunt interdum pretium. Suspendisse eleifend est tellus, id semper dolor ultrices vel. Interdum et malesuada fames ac ante ipsum primis in faucibus"


lorem75 : String
lorem75 =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut porta libero ut sapien tempus, at finibus urna volutpat. Nunc erat libero, vulputate sed odio sed, porttitor egestas turpis. Nam efficitur auctor varius. Donec ante libero, feugiat sit amet elit ac, aliquam malesuada nibh. Proin tempor, quam ac mollis dictum, sapien eros pulvinar neque, porta feugiat velit lectus at elit. Fusce cursus purus nibh, a suscipit diam imperdiet sit amet. Integer a felis non velit convallis."


lorem50 : String
lorem50 =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam eu vestibulum nisl. Aliquam nec dui sem. Vivamus sit amet purus velit. Morbi sit amet lorem non magna euismod imperdiet quis sed magna. Duis id leo pharetra, elementum libero ut, placerat nisi. Nulla in mauris laoreet, eleifend elit sit amet, porta."


lorem25 : String
lorem25 =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur ut molestie risus, eu lacinia justo. Nunc elit nunc, pellentesque quis vestibulum vel, volutpat nec elit."


lorem n =
    List.foldr (++) "" (List.repeat n lorem25)
