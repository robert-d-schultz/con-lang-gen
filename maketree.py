from ete3 import Tree, TreeStyle, Tree, TextFace, NodeStyle, add_face_to_node
import sys
from collections import OrderedDict
import os

# build list of filenames
filenames = []
for d in os.listdir('out/'):
    filenames.append('out/' + d + '/')

# roman numeral thing just incase things get crowded...
def write_roman(num):
    roman = OrderedDict()
    roman[1000] = "M"
    roman[900] = "CM"
    roman[500] = "D"
    roman[400] = "CD"
    roman[100] = "C"
    roman[90] = "XC"
    roman[50] = "L"
    roman[40] = "XL"
    roman[10] = "X"
    roman[9] = "IX"
    roman[5] = "V"
    roman[4] = "IV"
    roman[1] = "I"
    def roman_num(num):
        for r in roman.keys():
            x, y = divmod(num, r)
            yield roman[r] * x
            num -= (r * x)
            if num > 0:
                roman_num(num)
            else:
                break
    return "".join([a for a in roman_num(num)])

for filename in filenames:
    if not os.path.exists(filename + 'tree_map.png'):
        t_str = open(filename + 'tree_map.html', 'r').read()
        t = Tree(t_str, format=1)

        ts = TreeStyle()
        ts.show_leaf_name = False
        ts.mode = "c"
        ts.show_scale = False
        ts.optimal_scale_level = "full"
        #ts.arc_start = -180 # 0 degrees = 3 o'clock
        #ts.arc_span = 180
        def my_layout(node):
            F = TextFace(node.name, tight_text=False)
            F.margin_top = 1
            F.margin_right = 5
            F.margin_left = 5
            add_face_to_node(F, node, column=0, position="branch-bottom")

        ts.layout_fn = my_layout

        nstyle = NodeStyle()
        nstyle["size"] = 15
        nstyle["hz_line_width"] = 2
        nstyle["vt_line_width"] = 2

        i = 1
        for n in t.traverse():
           n.set_style(nstyle)
           #ts.legend.add_face(TextFace(write_roman(i).lower() + ". "), column=0)
           #ts.legend.add_face(TextFace(n.name), column=1)
           #n.name = write_roman(i).lower()
           i += 1

        #t.render("mytree.pdf", w=8.5, units="in", tree_style=ts)
        t.render(os.path.split(filename)[0] + "/mytree.png", h=1080, units="px", tree_style=ts)
    else:
        print("skipping " + os.path.split(filename)[0])
