module Day20Spec where

import Data.Map (Map, fromList)
import Day20
import Day20 (Image)
import Test.Hspec

testInput = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###"

parsedInput =
  ( [False, False, True, False, True, False, False, True, True, True, True, True, False, True, False, True, False, True, False, True, True, True, False, True, True, False, False, False, False, False, True, True, True, False, True, True, False, True, False, False, True, True, True, False, True, True, True, True, False, False, True, True, True, True, True, False, False, True, False, False, False, False, True, False, False, True, False, False, True, True, False, False, True, True, True, False, False, True, True, True, True, True, True, False, True, True, True, False, False, False, True, True, True, True, False, False, True, False, False, True, True, True, True, True, False, False, True, True, False, False, True, False, True, True, True, True, True, False, False, False, True, True, False, True, False, True, False, False, True, False, True, True, False, False, True, False, True, False, False, False, False, False, False, True, False, True, True, True, False, True, True, True, True, True, True, False, True, True, True, False, True, True, True, True, False, False, False, True, False, True, True, False, True, True, False, False, True, False, False, True, False, False, True, True, True, True, True, False, False, False, False, False, True, False, True, False, False, False, False, True, True, True, False, False, True, False, True, True, False, False, False, False, False, False, True, False, False, False, False, False, True, False, False, True, False, False, True, False, False, True, True, False, False, True, False, False, False, True, True, False, True, True, True, True, True, True, False, True, True, True, True, False, True, True, True, True, False, True, False, True, False, False, False, True, False, False, False, False, False, False, False, True, False, False, True, False, True, False, True, False, False, False, True, True, True, True, False, True, True, False, True, False, False, False, False, False, False, True, False, False, True, False, False, False, True, True, False, True, False, True, True, False, False, True, False, False, False, True, True, False, True, False, True, True, False, False, True, True, True, False, True, False, False, False, False, False, False, True, False, True, False, False, False, False, False, False, False, True, False, True, False, True, False, True, True, True, True, False, True, True, True, False, True, True, False, False, False, True, False, False, False, False, False, True, True, True, True, False, True, False, False, True, False, False, True, False, True, True, False, True, False, False, False, False, True, True, False, False, True, False, True, True, True, True, False, False, False, False, True, True, False, False, False, True, True, False, False, True, False, False, False, True, False, False, False, False, False, False, True, False, True, False, False, False, False, False, False, False, True, False, False, False, False, False, False, False, True, True, False, False, True, True, True, True, False, False, True, False, False, False, True, False, True, False, True, False, False, False, True, True, False, False, True, False, True, False, False, True, True, True, False, False, True, True, True, True, True, False, False, False, False, False, False, False, False, True, False, False, True, True, True, True, False, False, False, False, False, False, True, False, False, True],
    [[True, False, False, True, False], [True, False, False, False, False], [True, True, False, False, True], [False, False, True, False, False], [False, False, True, True, True]]
  )

testImageBits = [[True, False, False, True, False], [True, False, False, False, False], [True, True, False, False, True], [False, False, True, False, False], [False, False, True, True, True]]

testImageEnhancementAlgorithm :: ImageEnhancementAlgorithm
testImageEnhancementAlgorithm = fromList [(0, False), (1, False), (2, True), (3, False), (4, True), (5, False), (6, False), (7, True), (8, True), (9, True), (10, True), (11, True), (12, False), (13, True), (14, False), (15, True), (16, False), (17, True), (18, False), (19, True), (20, True), (21, True), (22, False), (23, True), (24, True), (25, False), (26, False), (27, False), (28, False), (29, False), (30, True), (31, True), (32, True), (33, False), (34, True), (35, True), (36, False), (37, True), (38, False), (39, False), (40, True), (41, True), (42, True), (43, False), (44, True), (45, True), (46, True), (47, True), (48, False), (49, False), (50, True), (51, True), (52, True), (53, True), (54, True), (55, False), (56, False), (57, True), (58, False), (59, False), (60, False), (61, False), (62, True), (63, False), (64, False), (65, True), (66, False), (67, False), (68, True), (69, True), (70, False), (71, False), (72, True), (73, True), (74, True), (75, False), (76, False), (77, True), (78, True), (79, True), (80, True), (81, True), (82, True), (83, False), (84, True), (85, True), (86, True), (87, False), (88, False), (89, False), (90, True), (91, True), (92, True), (93, True), (94, False), (95, False), (96, True), (97, False), (98, False), (99, True), (100, True), (101, True), (102, True), (103, True), (104, False), (105, False), (106, True), (107, True), (108, False), (109, False), (110, True), (111, False), (112, True), (113, True), (114, True), (115, True), (116, True), (117, False), (118, False), (119, False), (120, True), (121, True), (122, False), (123, True), (124, False), (125, True), (126, False), (127, False), (128, True), (129, False), (130, True), (131, True), (132, False), (133, False), (134, True), (135, False), (136, True), (137, False), (138, False), (139, False), (140, False), (141, False), (142, False), (143, True), (144, False), (145, True), (146, True), (147, True), (148, False), (149, True), (150, True), (151, True), (152, True), (153, True), (154, True), (155, False), (156, True), (157, True), (158, True), (159, False), (160, True), (161, True), (162, True), (163, True), (164, False), (165, False), (166, False), (167, True), (168, False), (169, True), (170, True), (171, False), (172, True), (173, True), (174, False), (175, False), (176, True), (177, False), (178, False), (179, True), (180, False), (181, False), (182, True), (183, True), (184, True), (185, True), (186, True), (187, False), (188, False), (189, False), (190, False), (191, False), (192, True), (193, False), (194, True), (195, False), (196, False), (197, False), (198, False), (199, True), (200, True), (201, True), (202, False), (203, False), (204, True), (205, False), (206, True), (207, True), (208, False), (209, False), (210, False), (211, False), (212, False), (213, False), (214, True), (215, False), (216, False), (217, False), (218, False), (219, False), (220, True), (221, False), (222, False), (223, True), (224, False), (225, False), (226, True), (227, False), (228, False), (229, True), (230, True), (231, False), (232, False), (233, True), (234, False), (235, False), (236, False), (237, True), (238, True), (239, False), (240, True), (241, True), (242, True), (243, True), (244, True), (245, True), (246, False), (247, True), (248, True), (249, True), (250, True), (251, False), (252, True), (253, True), (254, True), (255, True), (256, False), (257, True), (258, False), (259, True), (260, False), (261, False), (262, False), (263, True), (264, False), (265, False), (266, False), (267, False), (268, False), (269, False), (270, False), (271, True), (272, False), (273, False), (274, True), (275, False), (276, True), (277, False), (278, True), (279, False), (280, False), (281, False), (282, True), (283, True), (284, True), (285, True), (286, False), (287, True), (288, True), (289, False), (290, True), (291, False), (292, False), (293, False), (294, False), (295, False), (296, False), (297, True), (298, False), (299, False), (300, True), (301, False), (302, False), (303, False), (304, True), (305, True), (306, False), (307, True), (308, False), (309, True), (310, True), (311, False), (312, False), (313, True), (314, False), (315, False), (316, False), (317, True), (318, True), (319, False), (320, True), (321, False), (322, True), (323, True), (324, False), (325, False), (326, True), (327, True), (328, True), (329, False), (330, True), (331, False), (332, False), (333, False), (334, False), (335, False), (336, False), (337, True), (338, False), (339, True), (340, False), (341, False), (342, False), (343, False), (344, False), (345, False), (346, False), (347, True), (348, False), (349, True), (350, False), (351, True), (352, False), (353, True), (354, True), (355, True), (356, True), (357, False), (358, True), (359, True), (360, True), (361, False), (362, True), (363, True), (364, False), (365, False), (366, False), (367, True), (368, False), (369, False), (370, False), (371, False), (372, False), (373, True), (374, True), (375, True), (376, True), (377, False), (378, True), (379, False), (380, False), (381, True), (382, False), (383, False), (384, True), (385, False), (386, True), (387, True), (388, False), (389, True), (390, False), (391, False), (392, False), (393, False), (394, True), (395, True), (396, False), (397, False), (398, True), (399, False), (400, True), (401, True), (402, True), (403, True), (404, False), (405, False), (406, False), (407, False), (408, True), (409, True), (410, False), (411, False), (412, False), (413, True), (414, True), (415, False), (416, False), (417, True), (418, False), (419, False), (420, False), (421, True), (422, False), (423, False), (424, False), (425, False), (426, False), (427, False), (428, True), (429, False), (430, True), (431, False), (432, False), (433, False), (434, False), (435, False), (436, False), (437, False), (438, True), (439, False), (440, False), (441, False), (442, False), (443, False), (444, False), (445, False), (446, True), (447, True), (448, False), (449, False), (450, True), (451, True), (452, True), (453, True), (454, False), (455, False), (456, True), (457, False), (458, False), (459, False), (460, True), (461, False), (462, True), (463, False), (464, True), (465, False), (466, False), (467, False), (468, True), (469, True), (470, False), (471, False), (472, True), (473, False), (474, True), (475, False), (476, False), (477, True), (478, True), (479, True), (480, False), (481, False), (482, True), (483, True), (484, True), (485, True), (486, True), (487, False), (488, False), (489, False), (490, False), (491, False), (492, False), (493, False), (494, False), (495, True), (496, False), (497, False), (498, True), (499, True), (500, True), (501, True), (502, False), (503, False), (504, False), (505, False), (506, False), (507, False), (508, True), (509, False), (510, False), (511, True)]

testImage :: Image
testImage =
  ( (5, 5),
    fromList
      [ ((0, 0), 18),
        ((0, 1), 147),
        ((0, 2), 152),
        ((0, 3), 192),
        ((0, 4), 0),
        ((1, 0), 36),
        ((1, 1), 294),
        ((1, 2), 305),
        ((1, 3), 393),
        ((1, 4), 72),
        ((2, 0), 8),
        ((2, 1), 68),
        ((2, 2), 34),
        ((2, 3), 275),
        ((2, 4), 152),
        ((3, 0), 16),
        ((3, 1), 129),
        ((3, 2), 12),
        ((3, 3), 103),
        ((3, 4), 312),
        ((4, 0), 32),
        ((4, 1), 258),
        ((4, 2), 16),
        ((4, 3), 134),
        ((4, 4), 48)
      ]
  )

spec :: Spec
spec = do
  it "parseInput" $ do
    parseInput testInput
      `shouldBe` parsedInput

  it "imageEnhancementAlgorithmFromBits" $ do
    imageEnhancementAlgorithmFromBits (fst parsedInput) `shouldBe` testImageEnhancementAlgorithm

  it "imageFromBits" $ do
    imageFromBits testImageBits `shouldBe` testImage

  it "extendImage" $ do
    extendImage testImage
      `shouldBe` ( (7, 7),
                   fromList
                     [ ((0, 0), 1),
                       ((0, 1), 9),
                       ((0, 2), 73),
                       ((0, 3), 72),
                       ((0, 4), 64),
                       ((0, 5), 0),
                       ((0, 6), 0),
                       ((1, 0), 2),
                       ((1, 1), 18),
                       ((1, 2), 147),
                       ((1, 3), 152),
                       ((1, 4), 192),
                       ((1, 5), 0),
                       ((1, 6), 0),
                       ((2, 0), 4),
                       ((2, 1), 36),
                       ((2, 2), 294),
                       ((2, 3), 305),
                       ((2, 4), 393),
                       ((2, 5), 72),
                       ((2, 6), 64),
                       ((3, 0), 1),
                       ((3, 1), 8),
                       ((3, 2), 68),
                       ((3, 3), 34),
                       ((3, 4), 275),
                       ((3, 5), 152),
                       ((3, 6), 192),
                       ((4, 0), 2),
                       ((4, 1), 16),
                       ((4, 2), 129),
                       ((4, 3), 12),
                       ((4, 4), 103),
                       ((4, 5), 312),
                       ((4, 6), 448),
                       ((5, 0), 4),
                       ((5, 1), 32),
                       ((5, 2), 258),
                       ((5, 3), 16),
                       ((5, 4), 134),
                       ((5, 5), 48),
                       ((5, 6), 384),
                       ((6, 0), 0),
                       ((6, 1), 0),
                       ((6, 2), 4),
                       ((6, 3), 32),
                       ((6, 4), 260),
                       ((6, 5), 32),
                       ((6, 6), 256)
                     ]
                 )
    (extendImage . extendImage $ testImage)
      `shouldBe` ( (9, 9),
                   fromList
                     [ ((0, 0), 0),
                       ((0, 1), 0),
                       ((0, 2), 0),
                       ((0, 3), 0),
                       ((0, 4), 0),
                       ((0, 5), 0),
                       ((0, 6), 0),
                       ((0, 7), 0),
                       ((0, 8), 0),
                       ((1, 0), 0),
                       ((1, 1), 1),
                       ((1, 2), 9),
                       ((1, 3), 73),
                       ((1, 4), 72),
                       ((1, 5), 64),
                       ((1, 6), 0),
                       ((1, 7), 0),
                       ((1, 8), 0),
                       ((2, 0), 0),
                       ((2, 1), 2),
                       ((2, 2), 18),
                       ((2, 3), 147),
                       ((2, 4), 152),
                       ((2, 5), 192),
                       ((2, 6), 0),
                       ((2, 7), 0),
                       ((2, 8), 0),
                       ((3, 0), 0),
                       ((3, 1), 4),
                       ((3, 2), 36),
                       ((3, 3), 294),
                       ((3, 4), 305),
                       ((3, 5), 393),
                       ((3, 6), 72),
                       ((3, 7), 64),
                       ((3, 8), 0),
                       ((4, 0), 0),
                       ((4, 1), 1),
                       ((4, 2), 8),
                       ((4, 3), 68),
                       ((4, 4), 34),
                       ((4, 5), 275),
                       ((4, 6), 152),
                       ((4, 7), 192),
                       ((4, 8), 0),
                       ((5, 0), 0),
                       ((5, 1), 2),
                       ((5, 2), 16),
                       ((5, 3), 129),
                       ((5, 4), 12),
                       ((5, 5), 103),
                       ((5, 6), 312),
                       ((5, 7), 448),
                       ((5, 8), 0),
                       ((6, 0), 0),
                       ((6, 1), 4),
                       ((6, 2), 32),
                       ((6, 3), 258),
                       ((6, 4), 16),
                       ((6, 5), 134),
                       ((6, 6), 48),
                       ((6, 7), 384),
                       ((6, 8), 0),
                       ((7, 0), 0),
                       ((7, 1), 0),
                       ((7, 2), 0),
                       ((7, 3), 4),
                       ((7, 4), 32),
                       ((7, 5), 260),
                       ((7, 6), 32),
                       ((7, 7), 256),
                       ((7, 8), 0),
                       ((8, 0), 0),
                       ((8, 1), 0),
                       ((8, 2), 0),
                       ((8, 3), 0),
                       ((8, 4), 0),
                       ((8, 5), 0),
                       ((8, 6), 0),
                       ((8, 7), 0),
                       ((8, 8), 0)
                     ]
                 )

  it "enhanceImage" $ do
    enhanceImage testImageEnhancementAlgorithm testImage
      `shouldBe` ( (7, 7),
                   fromList
                     [ ((0, 0), 10),
                       ((0, 1), 83),
                       ((0, 2), 155),
                       ((0, 3), 217),
                       ((0, 4), 200),
                       ((0, 5), 64),
                       ((0, 6), 0),
                       ((1, 0), 28),
                       ((1, 1), 230),
                       ((1, 2), 311),
                       ((1, 3), 442),
                       ((1, 4), 465),
                       ((1, 5), 136),
                       ((1, 6), 64),
                       ((2, 0), 49),
                       ((2, 1), 397),
                       ((2, 2), 111),
                       ((2, 3), 380),
                       ((2, 4), 483),
                       ((2, 5), 281),
                       ((2, 6), 200),
                       ((3, 0), 42),
                       ((3, 1), 338),
                       ((3, 2), 150),
                       ((3, 3), 177),
                       ((3, 4), 398),
                       ((3, 5), 114),
                       ((3, 6), 400),
                       ((4, 0), 29),
                       ((4, 1), 236),
                       ((4, 2), 356),
                       ((4, 3), 291),
                       ((4, 4), 284),
                       ((4, 5), 229),
                       ((4, 6), 296),
                       ((5, 0), 50),
                       ((5, 1), 401),
                       ((5, 2), 137),
                       ((5, 3), 78),
                       ((5, 4), 113),
                       ((5, 5), 394),
                       ((5, 6), 80),
                       ((6, 0), 36),
                       ((6, 1), 290),
                       ((6, 2), 274),
                       ((6, 3), 148),
                       ((6, 4), 162),
                       ((6, 5), 276),
                       ((6, 6), 160)
                     ]
                 )

  it "countLit" $ do
    countLit testImage `shouldBe` 10
    countLit (extendImage $ testImage) `shouldBe` 10
    countLit (extendImage . extendImage $ testImage) `shouldBe` 10

  it "howManyPixelsAreLit" $ do
    howManyPixelsAreLit testInput `shouldBe` 35

main = hspec spec
