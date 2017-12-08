module Day08 exposing (main)

import Answer
import Dict exposing (Dict)
import List.Extra
import Regex


type alias Registers =
    Dict String Int


type alias Instruction =
    { reg : String
    , op : String
    , val : Int
    , ifreg : String
    , ifop : String
    , ifval : Int
    }


parseInstruction : String -> Instruction
parseInstruction line =
    let
        matches =
            Regex.find Regex.All (Regex.regex "(\\w+) (\\w+) (-?\\d+) if (\\w+) (\\S+) (-?\\d+)") (Debug.log "line" line)

        submatches =
            matches |> List.head |> Maybe.withDefault (Regex.Match "" [] 0 0) |> .submatches |> List.map (Maybe.withDefault "")

        reg =
            submatches |> List.Extra.getAt 0 |> Maybe.withDefault ""

        op =
            submatches |> List.Extra.getAt 1 |> Maybe.withDefault ""

        val =
            submatches |> List.Extra.getAt 2 |> Maybe.withDefault "" |> String.toInt |> Result.withDefault 0

        ifreg =
            submatches |> List.Extra.getAt 3 |> Maybe.withDefault ""

        ifop =
            submatches |> List.Extra.getAt 4 |> Maybe.withDefault ""

        ifval =
            submatches |> List.Extra.getAt 5 |> Maybe.withDefault "" |> String.toInt |> Result.withDefault 0
    in
        Debug.log "inst" (Instruction reg op val ifreg ifop ifval)


parseOp : String -> (comparable -> comparable -> Int)
parseOp op =
    case op of
        "inc" ->
            (+)

        "dec" ->
            (-)

        _ ->
            (\a b -> a)


parseIfOp : String -> (comparable -> comparable -> Bool)
parseIfOp op =
    case op of
        "==" ->
            (==)

        "!=" ->
            (/=)

        ">" ->
            (>)

        "<" ->
            (<)

        ">=" ->
            (>=)

        "<=" ->
            (<=)

        _ ->
            (\a b -> False)


execInstruction : Registers -> Instruction -> Registers
execInstruction registers instruction =
    let
        val =
            registers
                |> Dict.get instruction.reg
                |> Maybe.withDefault 0

        ifval =
            registers
                |> Dict.get instruction.ifreg
                |> Maybe.withDefault 0

        op =
            parseOp instruction.op

        ifOp =
            parseIfOp instruction.ifop
    in
        if ifOp ifval instruction.ifval then
            Dict.insert instruction.reg (op val instruction.val) registers
        else
            registers



-- Part 1


execInstructions : Registers -> List Instruction -> Registers
execInstructions registers instructions =
    case instructions of
        head :: tail ->
            execInstructions (execInstruction registers head) tail

        [] ->
            registers



-- Part 2


execInstructions2 : Registers -> Int -> List Instruction -> ( Registers, Int )
execInstructions2 registers maxValue instructions =
    case instructions of
        head :: tail ->
            let
                nextRegisters =
                    execInstruction registers head

                nextMaxValue =
                    nextRegisters
                        |> Dict.values
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
                execInstructions2 nextRegisters (max maxValue nextMaxValue) tail

        [] ->
            ( registers, maxValue )



-- Main


input =
    "o inc 394 if tcy >= -3\ns inc -204 if s <= -5\nkeq inc -34 if keq > -6\nip inc 762 if ip == 4\nzr inc -644 if ip >= 6\nmyk dec 894 if yq == -10\npc dec -990 if keq > -44\nhwk inc 106 if qfz == -5\no inc 406 if qfz != -3\nljs dec 34 if myk == 0\nn inc 579 if n != -6\nsd dec -645 if pc <= 992\now inc 154 if bio == 0\nige dec 108 if ip >= -9\npy dec 799 if vt == 0\nqfz dec -943 if tcy <= -9\nhwk inc 569 if o == 800\ntcy inc 90 if gij < 3\nqpa inc -15 if ip < 1\nkeq inc -2 if u == 0\nqpa inc -132 if keq <= -38\nhwk inc -87 if zr == 7\nige dec 759 if g >= 9\ncy inc -232 if scy > -8\nhwk dec 539 if vt >= -7\nd inc 517 if d <= 8\nkeq inc -65 if d < 509\nsd dec -585 if vt >= -1\nzr dec 970 if vt < 6\nyq dec -670 if n == 579\nqpa inc -839 if sd != 1226\ns dec -633 if hwk > 26\nsd inc -310 if g > 6\nljs inc 764 if o != 790\nyq dec -785 if tcy >= 83\nqfz inc 136 if ljs == 721\nmyk inc -699 if n == 579\nqpa dec -16 if ow > 146\now dec -257 if ige >= -112\nd inc 675 if d != 517\ngij dec 294 if bio >= 1\nbio dec -937 if vt < 9\nmyk dec -630 if ige < -105\nqpa inc -630 if bio > 930\nip dec -280 if ljs >= 738\nhwk inc 532 if myk <= -61\nsd inc -71 if pc >= 987\nzr inc -753 if ip == -10\nbio inc 716 if scy >= -3\nu dec -59 if ljs >= 721\ntcy dec -872 if py <= -803\nsi dec -480 if py != -799\ncy dec -99 if gij <= 7\nbio inc 804 if d <= 508\ncy inc 489 if si > 0\no inc -910 if si <= 9\npc dec 393 if s <= 633\nzr inc -580 if zr != -967\nige inc -824 if qfz >= -7\nd dec -138 if o >= -116\nige inc -16 if scy < 2\ns dec -29 if ip != 6\npy inc -172 if bio <= 1657\nkeq inc 648 if tcy != 89\ncy dec 324 if n > 575\nljs inc 624 if myk <= -61\nn inc -643 if qpa == -1468\npy dec -637 if g != -2\nzr dec -174 if myk == -74\nhwk dec 746 if scy >= -4\nmyk dec 775 if pc < 602\nljs inc -490 if s < 670\nvt dec 173 if yq >= 1453\nbio inc -106 if bio <= 1661\nljs inc 654 if qfz <= 6\ngij dec -932 if d == 655\nige inc 146 if ljs == 1518\npy dec 294 if myk > -850\nzr inc 370 if n > -68\npc inc 463 if ige != -802\nsd inc -300 if d != 645\nip dec -523 if hwk < -178\nsd inc -652 if vt > -164\nyq dec -493 if d < 663\nljs inc -612 if o < -110\nmyk inc -211 if bio <= 1544\ngij dec -630 if pc < 598\nige inc -45 if ow <= 403\nkeq dec -658 if cy == -457\npy dec -171 if gij <= 1565\nhwk inc -159 if ljs < 1521\nsd inc 475 if qfz >= 4\nmyk inc 516 if ow >= 411\nhwk dec -812 if u < 53\nd inc 544 if ige >= -805\ncy inc -594 if ow >= 414\nzr inc -98 if tcy < 85\ncy dec -242 if sd == 859\nbio dec 177 if ljs == 1518\nmyk inc -485 if ow < 420\ns dec 451 if myk < -810\nqfz dec 865 if yq > 1954\nzr dec -430 if cy != -211\nyq dec -640 if py < -451\nqfz inc -886 if ljs == 1518\ngij inc -636 if keq > 1267\nbio inc 932 if ow >= 411\nqpa inc -638 if sd != 859\nqfz dec -726 if n != -61\ntcy dec 303 if yq <= 2579\nvt inc -915 if keq <= 1275\nn inc -653 if zr > -747\now inc -583 if keq <= 1277\nscy inc 522 if pc > 596\ngij inc -186 if n <= -56\nsd dec 908 if qpa < -1472\no dec -157 if si < 3\ng inc -239 if si < -9\nige inc -454 if hwk < -333\nqfz inc 992 if qpa >= -1470\npy dec -876 if sd > 852\npy inc 983 if scy >= 513\nvt inc -633 if yq <= 2592\no dec -167 if pc < 599\nyq inc -784 if ip < 529\nbio dec -179 if cy <= -209\nkeq dec -213 if keq <= 1278\ns inc 845 if u >= 56\nvt dec -890 if n <= -60\now dec 479 if myk > -815\ngij dec 93 if pc > 591\nqfz dec 976 if pc == 597\nmyk dec -402 if pc > 588\ncy dec -248 if s != 1054\nn dec -519 if ljs == 1518\now inc 323 if o < 224\now dec 386 if ljs > 1509\nscy inc 175 if ow != -715\ntcy inc -344 if ip > 525\no inc 200 if si <= 4\nip dec 942 if bio > 2477\nkeq dec 983 if sd < 864\no inc 776 if o < 420\nvt inc 284 if ljs > 1510\nqfz inc 272 if myk > -407\ntcy dec 276 if myk != -411\nsi inc -634 if keq == 500\ngij inc -767 if g == 0\no inc 857 if o > 1189\nyq inc -490 if cy != 27\nvt inc -646 if qfz != -153\nn dec 388 if n >= 447\nyq dec -734 if pc > 603\nljs inc 382 if vt <= -1187\nsi dec 933 if bio > 2477\nqpa dec -572 if ip == -419\nscy inc -520 if keq == 500\ng inc 133 if ljs <= 1908\now dec -77 if ige >= -1264\nsi dec 984 if yq >= 1305\nn dec 53 if pc == 597\nzr inc -682 if tcy >= 89\ng inc -839 if n > 16\nkeq inc 975 if qfz <= -145\nhwk inc -592 if u >= 51\ns inc -786 if gij != -113\nkeq dec -805 if n < 16\nige dec 854 if hwk < -940\nyq inc 656 if ow <= -643\nqfz inc -577 if pc <= 590\nd dec -85 if d < 1208\nkeq inc 546 if cy == 33\nbio dec 23 if ip == -419\nsi inc 205 if cy < 37\nige dec 979 if sd >= 855\no dec -482 if vt > -1198\ncy dec -72 if gij <= -112\nyq inc 276 if hwk > -930\nzr inc 994 if qfz > -153\nhwk inc 843 if py >= 1412\nige inc 468 if hwk >= -940\nsd inc 477 if myk < -406\nvt inc 557 if gij >= -124\ntcy inc 949 if ljs == 1900\nqpa dec 321 if g < 136\nscy inc 502 if pc < 603\nljs inc -339 if cy <= 95\ngij dec -963 if ljs < 1902\nmyk inc 1000 if bio >= 2449\nsd dec 317 if gij != 849\nqfz dec 148 if sd > 1027\ng dec -786 if s <= 279\npy dec -451 if ip != -422\nljs inc -756 if gij != 837\nd inc 459 if ljs > 1140\nhwk inc -371 if u > 58\nu inc 765 if qfz < -141\nljs inc -69 if yq != 1324\nip inc -347 if o <= 2530\ngij inc -56 if cy < 108\nzr inc 988 if qpa <= -1216\no dec -671 if zr < 552\nn inc -680 if qfz < -136\now dec -709 if sd < 1027\nip inc -73 if si <= -2352\nhwk dec 673 if gij <= 796\ns inc 806 if py != 1856\nzr dec -792 if g >= 910\nsi dec 325 if gij > 787\nu dec -950 if ow == 73\nyq dec 821 if sd <= 1025\ng dec -160 if myk == 589\nljs inc -870 if gij >= 795\now inc -309 if o == 3200\ns dec -472 if s == 1076\nip dec 510 if keq < 1852\nsd dec -599 if vt != -629\nige dec -147 if g > 1076\nsd dec 602 if hwk == -1979\nkeq dec 215 if ip <= -1286\ncy dec -290 if tcy >= 1038\npc inc 816 if g == 1079\ncy inc 171 if bio != 2467\nip inc 104 if d >= 1734\nscy inc -577 if qfz > -146\nn inc 209 if cy != 558\nqfz dec 130 if yq < 501\now dec 740 if ip <= -1165\nd dec -857 if vt > -639\npc inc -981 if bio < 2460\nzr dec -979 if ow == -972\nn inc -167 if ljs == 1071\nljs inc -315 if vt >= -636\nvt dec -276 if gij > 779\ng dec 778 if ip >= -1178\nsd inc -976 if tcy >= 1040\nd inc -517 if n > -467\nljs dec 359 if ow <= -976\ngij inc -811 if u >= 820\nljs dec -768 if scy != 102\nu dec -584 if zr < 1333\nqfz dec 588 if qfz > -268\nzr inc -775 if ow < -972\nmyk dec -693 if vt >= -367\ncy inc 937 if s > 1549\nhwk dec -686 if u >= 821\ntcy dec -868 if qfz != -276\nqpa inc 97 if scy > 111\ngij inc 296 if pc != 432\ncy dec -559 if o != 3199\ng dec 59 if ip < -1166\nkeq inc -262 if zr != 563\no dec 823 if py >= 1844\nljs dec -253 if g < 243\now inc -351 if tcy != 1914\nmyk dec -684 if d <= 2092\nzr dec -175 if qpa == -1221\ns inc -96 if scy <= 92\no inc 492 if keq <= 1589\ntcy inc 563 if n > -464\nljs inc 764 if s < 1553\nqpa inc -475 if qfz > -278\nhwk dec -474 if myk >= 1957\no dec -709 if vt >= -364\nip dec -372 if bio == 2458\nip dec 361 if qfz != -272\ntcy inc 986 if sd < 1019\now dec 405 if hwk >= -809\nn inc 894 if ow != -1336\ng dec 648 if cy != 1116\nsi inc -49 if gij == -24\nip dec -787 if myk <= 1974\nljs dec 449 if scy <= 111\now dec 940 if qpa != -1698\npc dec 788 if qfz > -277\nu inc -955 if bio <= 2465\nsd dec 113 if pc < -352\nn dec 827 if ow <= -2260\ntcy inc 298 if myk < 1973\ng inc -615 if ige > -1630\nige inc 561 if o != 3578\nscy dec -853 if myk > 1962\nige dec -248 if scy == 955\nip dec -961 if ige <= -1372\ngij dec 441 if bio >= 2456\ncy inc -631 if o == 3578\no dec -801 if gij == -465\nzr dec -313 if scy <= 946\ns dec -959 if yq == 493\ns inc 77 if g > -1023\nsd inc -160 if yq >= 485\nyq dec -274 if pc != -357\nd dec -882 if ige >= -1379\nvt dec 311 if o != 4383\nip inc 732 if n < -384\nljs inc 742 if py < 1854\nyq dec 707 if s < 2588\nsi inc -970 if cy != 486\ncy inc 867 if scy >= 961\now inc -880 if myk != 1972\npc inc -36 if ige <= -1369\ng dec 308 if qpa > -1698\nljs dec -900 if gij >= -474\nqpa inc 87 if hwk != -817\nzr inc -763 if py == 1853\ng dec 990 if myk <= 1975\ntcy dec -579 if vt < -672\nkeq dec -565 if pc >= -399\nu inc 381 if pc < -384\no dec -195 if bio > 2456\nmyk dec -39 if yq != 60\nqpa dec 416 if bio == 2458\nu inc 351 if keq < 2150\ns inc 854 if vt >= -668\nd dec 502 if myk < 1976\nscy inc 372 if qpa >= -2023\nhwk inc 362 if g == -2319\nkeq dec -972 if n != -390\npc inc 415 if tcy <= 3759\nn inc 37 if sd > 737\nn inc -617 if py != 1860\nyq dec -446 if yq <= 62\now inc -865 if pc == 23\nn inc -157 if ljs == 2619\nqfz inc 885 if keq != 2147\nige dec 106 if d <= 2460\nsd dec 409 if d < 2469\nige dec 219 if d >= 2454\ng dec -861 if qpa > -2029\ncy dec 216 if ljs == 2611\nljs dec -993 if qpa < -2027\nqfz inc 777 if d < 2467\nljs inc 348 if d != 2470\no dec -187 if bio > 2455\ngij inc -778 if d == 2463\nige inc 798 if ip != 1321\nn dec 160 if tcy < 3762\nip dec 431 if o > 4758\nkeq inc 889 if ip <= 890\nsi inc -272 if zr <= -192\nzr inc -289 if sd != 344\nsi inc 843 if o == 4761\nige inc -940 if ip >= 887\npc inc -189 if keq < 3045\no inc -658 if yq >= 501\nn dec -886 if keq >= 3045\ntcy inc -3 if o > 4095\now dec -639 if n < -1125\nn dec 81 if ip < 891\nqpa dec -430 if qfz <= 1394\ncy inc 602 if bio >= 2457\nqpa inc -579 if hwk >= -448\nqfz inc 559 if g > -1460\nu inc -902 if yq <= 507\npy dec 556 if s > 2578\nige dec 439 if bio != 2467\nhwk inc 327 if scy <= 1330\nd dec -634 if o > 4103\nmyk dec 919 if qfz < 1957\ntcy inc 680 if keq < 3050\nqpa dec 628 if cy == 880\nzr dec 112 if u <= -650\no dec -214 if si <= -2788\nkeq inc 132 if bio > 2451\nmyk inc 229 if qpa != -2219\nkeq inc 651 if n != -1203\nsi inc -655 if qfz != 1942\npy inc -726 if zr < -595\nscy inc -565 if py >= 567\nqfz dec -219 if vt >= -661\npy dec -405 if u > -656\nip dec 668 if g != -1454\nsd inc -496 if py == 976\nzr dec 315 if s != 2590\no dec -355 if myk >= 1047\ncy dec -575 if keq <= 3831\no dec 589 if qfz != 1947\nsd dec 487 if scy != 768\nqfz inc -742 if n == -1211\nn inc -81 if n > -1220\npc dec -747 if bio >= 2454\npc inc 814 if qpa == -2219\nmyk inc -874 if sd < -642\nmyk inc 375 if bio <= 2450\nu inc -7 if ip <= 229\nscy dec 677 if cy >= 1455\nkeq dec 746 if zr != -917\nyq inc 594 if hwk >= -121\nqpa inc -511 if bio != 2456\ntcy dec 24 if zr < -919\no inc 10 if vt >= -679\now dec -995 if yq != 506\npy dec -144 if tcy >= 4425\ng dec 501 if si > -3457\nsd inc 911 if scy > 82\ngij inc 545 if keq == 3080\ngij inc 498 if vt <= -671\nyq inc 728 if ip >= 227\nsi inc -751 if u > -669\nkeq dec 819 if d > 2453\now inc -863 if pc == 1395\nbio inc 651 if hwk < -134\ns inc -818 if gij != -200\nscy dec -26 if vt >= -675\now dec -279 if d > 2454\nn dec -451 if pc <= 1403\nmyk dec -113 if ljs != 2967\nmyk inc 14 if ip <= 210\nbio inc 473 if hwk < -126\nsd dec -907 if pc > 1392\nqfz dec 21 if sd <= 1170\ns inc 612 if vt != -664\nqpa inc -734 if o != 4686\nzr dec -487 if cy > 1453\nmyk dec -733 if sd > 1176\ntcy inc -873 if ip >= 220\ntcy inc -28 if vt <= -670\nscy dec -793 if qpa <= -3465\nu inc 301 if qfz >= 1180\nd inc 515 if g > -1950\nscy dec -335 if g > -1965\nhwk inc 563 if scy == 446\nbio dec -980 if py > 1112\nyq dec 504 if o <= 4683\nkeq dec 458 if scy > 443\no inc -840 if sd > 1159\npc inc 922 if g <= -1957\nsi dec -305 if ljs < 2960\nsi inc 576 if ip > 212\npc inc 167 if n == -841\nu dec 354 if d < 2470\npy dec -167 if qpa > -3469\nbio inc -599 if bio < 3912\nu dec -783 if s >= 3190\nhwk inc -838 if gij != -210\now dec 803 if keq >= 1803\now inc -592 if qpa > -3463\ngij dec 475 if d == 2463\ng dec -344 if u <= 69\nsi inc -575 if yq != -7\nige inc 247 if py != 1296\nscy inc -847 if g >= -1966\nzr inc 405 if yq > -5\nsi dec -102 if zr > -25\now dec -630 if ip <= 221\no inc 563 if si <= -3788\nqfz inc 722 if myk != 285\ngij inc 461 if ow > -4139\nbio inc -605 if cy < 1461\nige inc -235 if scy > -404\nzr dec -637 if n != -840\nn dec 847 if gij < -211\nkeq dec -165 if keq != 1806\no inc -27 if ige >= -2166\nd dec -751 if n <= -1681\no dec -847 if scy < -395\nhwk inc 526 if n < -1681\now dec -197 if o > 5234\no dec 730 if py != 1285\nljs inc 300 if o < 4503\nvt dec -530 if u > 66\nn dec 862 if zr >= 615\nsi inc 804 if py <= 1289\nmyk dec -87 if n == -2550\npc inc -633 if vt >= -141\nscy inc -392 if myk == 373\nscy dec 444 if scy != -793\ntcy inc 806 if zr < 624\nvt inc 957 if ip <= 212\nzr inc -380 if keq == 1968\nn inc 519 if ow <= -4124\nbio inc 738 if d != 3219\nhwk inc -599 if yq == 2\nip dec -795 if ige < -2155\ns inc -205 if myk == 373\nzr dec 36 if keq <= 1970\npc dec -69 if ige == -2160\nhwk dec 239 if tcy != 4333\nige dec 197 if s <= 3000\nu dec 564 if tcy != 4326\nu dec -109 if zr != 197\npy inc 61 if vt >= -147\ns dec -736 if tcy != 4330\nu inc 385 if n != -2030\nkeq dec 98 if s > 3726\nbio dec 467 if d == 3214\nsi dec 616 if ljs <= 3259\nscy inc -799 if o == 4495\nmyk inc -364 if u < 8\nqfz inc -180 if s != 3718\npy dec 954 if gij == -214\nqfz inc -608 if o >= 4487\nige dec -214 if ow < -4130\npy dec 31 if qpa > -3471\nd dec -625 if d <= 3210\nljs dec -407 if cy < 1460\nn dec -218 if py >= 355\nvt dec 577 if py == 363\nhwk dec 406 if cy != 1464\ng inc -243 if bio != 2968\nkeq inc -322 if vt > -727\ntcy inc -159 if bio < 2970\nqfz dec 208 if ljs > 3661\nu inc 130 if u != -2\nsi dec 283 if g == -2202\nhwk inc -489 if n <= -1812\nu inc -895 if gij < -219\nvt dec -375 if ljs < 3668\npc dec 325 if o == 4495\nqpa dec 655 if yq == 2\now inc 433 if sd <= 1172\nip inc 932 if ige != -2145\nqpa inc 256 if g == -2202\nige inc -839 if qfz <= 916\no inc 108 if yq <= 9\nmyk dec 49 if ige >= -2983\nd dec -825 if keq < 1556\nhwk dec 756 if ige >= -2989\ntcy dec 17 if u == 131\nkeq dec -841 if u >= 129\nscy inc -926 if si > -3897\nbio inc 880 if d > 4035\nsd inc -230 if qpa <= -3861\nsi dec 782 if n <= -1808\nmyk inc 998 if vt != -337\nhwk inc -363 if ljs < 3670\nqpa dec 349 if keq == 2389\nip dec 17 if s <= 3736\nu dec -985 if ige != -2984\nzr dec 470 if hwk == -2731\nige dec -767 if py <= 368\nyq dec 766 if keq >= 2385\nige dec 790 if yq == -758\ncy inc 712 if ip > 1939\nqfz inc 379 if ljs != 3666\npy inc -287 if ljs == 3666\ncy inc 148 if gij > -220\ncy dec -389 if ip >= 1925\ns dec 405 if g == -2202\nljs dec -95 if vt != -353\nsd dec -511 if qfz <= 912\nljs dec -997 if gij < -204\nscy dec -731 if ow != -3698\nyq inc -383 if bio >= 3853\nmyk inc -272 if tcy >= 4321\npy inc -162 if scy <= -2514\ng dec -975 if keq <= 2392\nbio dec -327 if n >= -1816\nu inc -309 if d == 4039\nscy inc -593 if cy > 1988\nbio dec 737 if cy >= 1990\no inc 978 if o == 4603\now inc -788 if bio <= 3448\nqfz inc 568 if qpa != -4212\ngij dec 637 if cy <= 1998\ncy dec -603 if n == -1821\nljs inc 642 if ip > 1925\nqpa inc 159 if ljs <= 5404\nip inc 987 if cy == 1992\nd dec -499 if ige > -2225\nn inc -267 if gij != -857\nu dec 396 if cy >= 1987\nhwk inc -131 if vt < -335\ntcy dec 878 if gij != -841\ncy dec -269 if d >= 4530\nsd dec 706 if si >= -4670\nn dec 239 if pc <= 1594\nsd dec 318 if zr >= -274\now dec 897 if s <= 3330\nzr inc -891 if si >= -4661\nu dec 119 if gij != -853\nkeq dec 427 if ige == -2215\nige dec 185 if pc < 1596\nn inc 200 if py >= -90\nscy inc 289 if u == 292\nscy dec -570 if ige < -2390\nmyk dec 676 if qpa == -4053\nyq dec 232 if sd < 434\nljs dec 685 if yq != -1379\nn dec -747 if ow >= -5389\nbio dec 900 if ige < -2398\nhwk inc 106 if qfz != 906\no inc -687 if u < 294\nsd inc 905 if sd > 430\nyq inc -405 if qpa == -4053\nu inc 871 if gij > -850\npc dec -145 if hwk > -2760\nsd dec -673 if u >= 284\nsi dec 407 if qpa > -4055\nip dec 491 if s < 3329\nbio inc 90 if ige != -2404\ng dec 429 if ljs > 5398\nqpa inc -965 if o >= 4888\ns dec 419 if myk > 279\ncy inc 309 if scy != -2261\nu inc -585 if pc <= 1740\nmyk inc 933 if ow >= -5382\nbio dec -959 if keq <= 1966\nzr inc -68 if d == 4538\ngij dec 418 if n <= -1127\ncy dec -537 if scy > -2260\nn dec -912 if scy <= -2251\nyq inc -880 if ow < -5382\nu dec -593 if yq <= -2663\nu dec -628 if qfz >= 907\nhwk dec -978 if keq == 1962\npy dec 286 if o > 4903\no dec 626 if vt >= -349\nvt dec 244 if si >= -5079\ng inc -282 if ip <= 2428\nzr dec 729 if py >= -86\npc dec 626 if vt >= -594\nmyk inc -630 if u == 928\nbio inc -318 if ige <= -2393\nhwk dec -636 if bio <= 3272\ngij inc 4 if tcy != 3437\nd inc -633 if keq != 1954\ncy dec -845 if qfz > 909\nbio dec -701 if zr < -1060\nbio dec 33 if bio <= 3985\ngij dec -770 if scy == -2252\no inc -301 if yq > -2673\ns inc -11 if s != 2903\nip inc -527 if yq > -2674\nd dec -700 if scy < -2245\no dec 157 if yq <= -2655\ng dec -914 if cy < 3962\nige inc -380 if s < 2908\nqpa inc -37 if hwk != -1784\npc inc 300 if keq == 1955\ns dec 259 if s < 2907\nyq inc 415 if cy < 3956\nd inc -713 if vt < -584\nsi inc -257 if ip < 1902\nn dec 573 if ljs > 5398\nn dec -227 if py == -84\ncy dec -469 if zr <= -1062\nqfz inc -210 if ige != -2779\nmyk dec -63 if ip < 1894\nkeq inc 573 if gij <= -486\ntcy dec 552 if keq < 2528\ns inc 340 if qpa == -5055\nzr inc 258 if qfz == 700\nkeq dec 528 if scy >= -2256\ng dec 876 if g < -1021\nsi dec -939 if si < -5340\ncy dec 200 if py > -80\nqfz inc -714 if o <= 3814\ng inc -497 if yq > -2253\nbio dec -953 if ow > -5384\ng dec 215 if ljs < 5396\nyq dec 24 if ip > 1906\nn inc 371 if scy <= -2248\no dec 264 if s < 2987\ns dec -428 if gij > -502\nige dec 629 if yq <= -2242\ngij inc -461 if g <= -2393\nd dec -217 if u > 926\ncy dec -781 if u != 926\nhwk dec 929 if tcy < 3450\nu dec 835 if ip >= 1897\ntcy dec 753 if u < 99\now inc 592 if py >= -91\nip dec -141 if pc != 1114\ntcy inc -295 if zr == -808\no inc -627 if d == 4109\ncy inc 469 if bio != 4905\nvt inc -913 if vt != -587\nsd dec 242 if zr > -809\nqfz dec -166 if g > -2407\ns dec 375 if yq >= -2253\nscy inc -741 if qpa != -5051\nmyk dec 358 if g >= -2406\nmyk dec 873 if u != 90\nhwk dec -387 if qpa == -5061\now dec -360 if n < -431\npy inc -907 if yq < -2239\npc dec -403 if myk >= -1576\nsd inc -788 if si > -5328\nqfz inc -95 if qfz > 158\ns inc -25 if si != -5337\ntcy inc -216 if qpa < -5054\now dec -542 if bio > 4899\ntcy dec 425 if hwk > -2707\ntcy inc -543 if ljs < 5404\nsd dec -200 if py < -989\nzr dec 421 if u <= 91\nd dec -522 if ljs != 5394\ncy inc -533 if ljs >= 5398\nvt inc -761 if py != -1001\nbio inc -888 if ige > -3419\nhwk inc -757 if gij == -956\nyq dec 982 if ip >= 1892\nscy dec 498 if myk != -1588\nsd dec -884 if o < 2922\nsi dec 630 if qpa < -5047\nbio dec -509 if gij == -947\nvt dec 811 if pc >= 1113\nyq inc 973 if ow <= -4257\ns inc 564 if zr != -805\nqpa dec 40 if ow > -4259\ntcy dec -628 if u >= 93\nqfz inc -951 if qpa != -5105\nsi dec 314 if py >= -994\nn dec 41 if scy >= -3497\nhwk inc -338 if pc > 1112\ncy inc -982 if g < -2387\now dec -651 if cy <= 4163\nsd inc 463 if g >= -2405\nyq dec -669 if qpa <= -5090\nzr inc -237 if ljs > 5393\npc inc 594 if zr != -1047\nbio inc -336 if keq <= 2001\npy inc 303 if s < 3586\ntcy dec 239 if gij != -965\ntcy dec -514 if g > -2407\ncy dec 362 if scy > -3493\nige dec 194 if o > 2917\ncy inc 547 if u != 91\nn inc 925 if si <= -6269\now dec -786 if py <= -687\ns inc 759 if zr != -1047\npy dec 844 if yq == -2562\ncy inc 640 if keq <= 2011\nzr inc 13 if d < 4634\nd dec -439 if o >= 2915\nu dec 616 if n >= 456\nmyk dec -754 if tcy > 2541\nsd dec 166 if vt == -2153\nbio inc -776 if vt >= -2161\nljs dec -616 if myk >= -1583\nzr inc 612 if ow < -2813\nvt dec 735 if ip >= 1896\nyq dec -856 if ige >= -3602\nzr dec 927 if py >= -1535\nu dec -416 if myk == -1579\npy dec -76 if sd < 2412\nmyk dec -48 if ip >= 1905\nqfz dec 95 if o > 2912\nvt dec 810 if d != 5070\nqfz inc 89 if qfz > -896\nqfz dec -527 if scy > -3500\nbio inc -851 if keq > 2005\nu inc -43 if pc != 1704\nu dec -143 if scy != -3487\ngij dec 947 if sd >= 2411\ng dec 202 if ow <= -2810\ntcy dec 438 if bio != 2384\nn inc -784 if sd <= 2400\nljs dec -64 if ip > 1893\nyq inc 951 if keq >= 2004\nbio dec -37 if vt == -2894\nige dec -280 if o > 2917\nqpa dec -373 if ige == -3323\nqpa dec 697 if yq > -1617\nscy dec -430 if s < 4329\nzr inc -253 if g == -2599\nkeq dec 339 if vt <= -2893\ng dec 703 if ljs <= 6078\nhwk dec -571 if vt == -2894\nige dec -954 if o <= 2928\nkeq dec 568 if o > 2914\nljs inc -645 if scy >= -3498\nkeq dec -827 if ljs != 5434\ncy dec 784 if g == -2599\ngij dec -47 if qpa == -5428\nhwk inc -260 if myk == -1579\nqpa dec -586 if g > -2608\no dec -581 if hwk > -3486\nqpa inc -86 if o == 2919\ngij dec -946 if zr < -2210\nu dec 212 if ow == -2812\now dec 268 if qpa < -4915\nqpa inc 867 if hwk <= -3487\npy dec -844 if hwk == -3491\ng inc 35 if ow != -3078\ngij dec -783 if vt <= -2885\nu dec 57 if tcy >= 2097\npc dec -465 if tcy >= 2103\nzr dec -262 if vt > -2904\nhwk inc 153 if myk < -1571\nsi dec 295 if tcy >= 2092\nu inc -59 if u < -270\nsd dec 18 if gij != 769\ngij inc -803 if bio < 2417\ncy inc 523 if myk < -1579\nvt dec -929 if ip < 1909\ns dec -584 if ow < -3079\nhwk dec -717 if keq < 1931\ngij inc -114 if sd != 2386\nqfz inc -478 if tcy != 2092\nsd inc -322 if cy < 4198\nige dec 271 if tcy >= 2106\nmyk inc -298 if g != -2564\nzr dec 711 if pc >= 1707\nsi dec -542 if sd < 2074\nvt dec 819 if scy <= -3493\nqfz inc 700 if ip >= 1891\ng inc 134 if sd < 2067\nqfz inc -572 if pc > 1703\now dec -770 if keq <= 1931\no inc -63 if bio != 2422\nhwk inc 797 if py < -612\nqpa dec 679 if si >= -6031\nqfz inc 46 if bio == 2422\ng dec 182 if g < -2423\nvt inc 488 if o != 2926\no dec -900 if cy > 4193\nzr inc -625 if d > 5070\nscy inc -64 if scy >= -3481\ns inc 958 if tcy >= 2091\nn inc -824 if myk != -1586\nn dec -601 if u > -338\nqfz inc 503 if s <= 5878\nhwk inc 445 if d >= 5074\now dec -123 if tcy != 2099\ns inc -160 if hwk >= -1831\nige inc -959 if pc == 1708\ns dec 526 if d > 5062\nsi inc -250 if pc > 1714\nscy dec -40 if n >= 233\now dec 909 if py == -614\now inc 147 if u <= -333\nsd inc -748 if pc > 1717\nqpa dec 303 if cy >= 4190\nqfz dec -347 if cy < 4206\nvt inc 575 if scy < -3458\now inc -59 if cy < 4201\now dec -701 if ip <= 1896\nqpa inc -666 if ige < -3318\ng dec -595 if ige == -3328\nsd dec 968 if u < -333\nd dec -135 if u > -334\nzr inc 501 if ljs != 5438\nqpa inc 126 if qfz >= 264\nip dec 334 if keq > 1926\nsi inc 979 if ljs >= 5427\nip dec -474 if vt > -1485\nvt dec -262 if pc <= 1710\nu dec -960 if ige == -3328\nscy inc -621 if ow < -3125\nzr inc -256 if cy > 4189\nu inc -133 if sd == 1096\nsi inc -53 if pc < 1706\nvt dec 695 if keq <= 1928\nzr inc 373 if hwk > -1833\nqpa dec -198 if ow >= -3139\ncy dec 636 if s != 5181\nip inc -732 if sd <= 1105\ntcy inc -756 if tcy <= 2091\nzr dec 503 if py <= -618\nyq inc 451 if ip > 1298\ngij dec 391 if zr < -2033\now dec -258 if tcy <= 2106\nyq inc 957 if g != -2022\ngij dec 639 if u < 502\nsd dec 253 if vt < -1908\ntcy inc -882 if cy < 3570\ns dec 556 if gij != -256\ncy dec -820 if keq == 1927\npy inc 885 if d < 5078\nige dec -207 if qpa != -5376\nscy dec -417 if py == 271\ng inc 45 if u >= 484\no dec -198 if o <= 3824\nd dec -978 if pc >= 1705\nscy dec -770 if ip != 1310\nhwk dec -35 if hwk > -1826\npc dec 470 if si > -5047\nn dec -141 if u > 483\now inc 244 if scy == -2885\npc dec 538 if d <= 6055\nsd inc 289 if ow >= -2638\nkeq inc -931 if ip != 1317\npy inc 746 if py != 278\nqfz dec -957 if si == -5051\nyq dec -374 if cy < 4377\nvt dec -749 if ljs >= 5426\nn inc 922 if bio != 2422\nscy dec 758 if s <= 4644\nqfz inc -739 if s <= 4635\nsi inc 653 if s >= 4634\nd inc 297 if ow == -2629\nvt inc 963 if zr != -2033\ng inc 977 if si <= -4393\nmyk dec -141 if qfz == 486\ns dec 68 if d == 6345\nzr inc 882 if s <= 4564\ncy inc -492 if pc == 1170\nmyk dec -492 if ow <= -2633\ns dec -186 if cy <= 3893\ntcy inc -947 if hwk == -1789\npc dec 455 if s > 4745\ngij dec 601 if n <= 370\nscy inc -688 if hwk < -1797\nscy inc 768 if qfz < 491\nmyk inc -187 if qfz != 490\ng dec -732 if ow != -2629\ncy dec 355 if qfz != 480\nip inc 901 if cy == 3534\nqfz dec 460 if yq != -203\npy dec 366 if d <= 6349\nqpa inc 474 if d > 6343\nkeq dec -768 if tcy == 270\ngij inc 876 if ljs >= 5427\ntcy inc 617 if bio > 2419\nsd inc -649 if ljs != 5441\now dec -20 if gij < 627\nqfz dec 436 if vt <= -198\npc dec -752 if d <= 6337\nip dec 928 if scy < -2867\nqpa inc 780 if d > 6344\nbio inc 963 if n == 379\nige inc 93 if gij != 620\nige inc -185 if scy == -2875\npy inc -650 if qpa >= -4129\nip dec -895 if gij < 621\ns dec -539 if ip != 2180\npy dec 71 if qpa >= -4127\nbio dec 229 if gij != 619\nyq dec 56 if vt > -205\nqfz dec -389 if sd != 485\ns dec -85 if cy < 3540\nip inc 386 if yq < -253\ngij dec 571 if qpa != -4119\nip inc -645 if bio == 3380\nqpa inc -822 if qpa < -4118\nljs inc -43 if myk == -1625\nbio inc -462 if tcy == 887\nmyk inc -717 if vt > -200\nscy dec 228 if myk != -2348\nmyk dec -518 if myk <= -2336\nd inc -593 if si < -4397\nip inc -912 if ige < -3428\nsi inc -320 if vt <= -196\nn dec 997 if ige != -3420\nip inc -801 if ip > 2557\npc dec -526 if hwk < -1781\nige dec -279 if ip <= 1762\now dec -488 if qfz >= 436\nscy inc 408 if qfz != 443\ngij inc -992 if scy <= -2691\npc inc 238 if ige != -3143\ng inc 760 if scy >= -2697\ns dec -57 if hwk <= -1783\nsi dec 368 if ip == 1760\nqfz dec 760 if vt >= -205\nmyk dec 254 if u == 491\nsd inc -466 if myk <= -1818\ncy dec -64 if qfz != -331\nvt dec -632 if qpa == -4944\ngij inc 382 if zr == -2042\npy inc 585 if zr <= -2050\nn dec 612 if d < 5760\no inc 673 if py <= -62\nu inc 986 if py <= -61\nip dec 128 if gij == -954\nu inc -173 if ige <= -3140\nip inc 358 if scy <= -2687\no inc 114 if py > -67\nhwk dec -413 if n <= -233\nscy dec 979 if s == 5434\nmyk inc -190 if py <= -62\nu dec 884 if hwk != -1376\nsd inc -711 if zr > -2038\npc inc -292 if o <= 4690\nd inc -490 if d < 5749\npy dec 812 if ljs <= 5397\nkeq dec -74 if qfz >= -324\nqfz dec 820 if ige >= -3149\now dec 629 if d >= 5746\nscy dec 481 if qfz != -1144\ngij dec 272 if ip == 2118\nkeq inc -957 if scy == -4161\nljs dec -974 if hwk == -1376\ngij dec 848 if sd < 20\no inc -718 if scy >= -4160\ns dec 807 if g <= -244\nzr inc -12 if sd != 23\nip dec 332 if sd >= 19\nsd inc -237 if ljs >= 6370\no dec 105 if ow == -2750\nu inc -466 if qpa > -4950\ncy inc -11 if keq >= 1846\ngij dec 576 if scy > -4159\npy inc -470 if u == 834\nqfz inc 812 if vt != 434\nyq inc -447 if bio <= 2927\nqpa inc 983 if bio != 2926\ng dec -396 if py != -882\nqfz dec -504 if vt < 437\nd inc 202 if ip > 2121\nip dec -112 if n < -230\nhwk inc -295 if keq <= 1845\nhwk dec -244 if d == 5752\ntcy inc 905 if n != -235\nljs dec 901 if yq >= -710\nkeq dec -871 if sd <= 20\nu inc -900 if s == 5434\npc dec 916 if vt <= 426"


part1 =
    input
        |> String.lines
        |> List.map parseInstruction
        |> execInstructions Dict.empty
        |> Dict.values
        |> List.maximum
        |> Maybe.withDefault 0


part2 =
    input
        |> String.lines
        |> List.map parseInstruction
        |> execInstructions2 Dict.empty 0
        |> Tuple.second


main =
    Answer.render part1 part2
