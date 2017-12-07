module Day07 exposing (main)

import Answer
import Dict exposing (Dict)
import List.Extra
import Regex


type alias Tower =
    Dict String TowerNode


type alias TowerNode =
    { name : String
    , weight : Int
    , subnodes : List String
    }


emptyNode =
    TowerNode "" 0 []


parseTowerNode : String -> TowerNode
parseTowerNode line =
    let
        matches =
            Regex.find Regex.All (Regex.regex "([a-z]+) \\((\\d+)\\)( -> (.*))?") (Debug.log "line" line)

        submatches =
            matches |> List.head |> Maybe.withDefault (Regex.Match "" [] 0 0) |> .submatches |> List.map (Maybe.withDefault "")

        node =
            submatches |> List.Extra.getAt 0 |> Maybe.withDefault ""

        weight =
            submatches |> List.Extra.getAt 1 |> Maybe.withDefault "" |> String.toInt |> Result.withDefault 0

        subnodes =
            submatches |> List.Extra.getAt 3 |> Maybe.map (String.split ", ") |> Maybe.withDefault [] |> List.filter (\e -> not (String.isEmpty e))
    in
        Debug.log "node" { name = node, weight = weight, subnodes = subnodes }


parseTower : Tower -> List TowerNode -> Tower
parseTower tower nodes =
    case nodes of
        head :: tail ->
            parseTower (Dict.insert head.name head tower) tail

        [] ->
            tower


parseCumulativeTower : Tower -> String -> List TowerNode -> List TowerNode -> ( Tower, String )
parseCumulativeTower tower unbalancedNode skippedNodes unprocessedNodes =
    case unprocessedNodes of
        head :: tail ->
            let
                subnodesParsed =
                    head.subnodes |> List.all (\n -> Dict.member n tower)

                subnodeWeights =
                    head.subnodes
                        |> List.map (\n -> Dict.get n tower)
                        |> List.map (Maybe.withDefault emptyNode)
                        |> List.map .weight

                subnodeWeightsSummed =
                    subnodeWeights
                        |> List.sum

                subnodeWeightsWrong =
                    (subnodeWeights |> List.Extra.unique |> List.length) > 1

                nextUnbalancedNode =
                    if String.isEmpty unbalancedNode && subnodeWeightsWrong then
                        head.name
                    else
                        unbalancedNode
            in
                if subnodesParsed then
                    parseCumulativeTower
                        (Dict.insert head.name { head | weight = head.weight + subnodeWeightsSummed } tower)
                        nextUnbalancedNode
                        skippedNodes
                        tail
                else
                    parseCumulativeTower tower unbalancedNode (head :: skippedNodes) tail

        [] ->
            if List.isEmpty skippedNodes then
                ( tower, unbalancedNode )
            else
                parseCumulativeTower tower unbalancedNode [] skippedNodes



-- Part 1


findRoot : Tower -> String
findRoot tower =
    tower
        |> Dict.values
        |> List.sortBy .weight
        |> List.Extra.last
        |> Maybe.withDefault emptyNode
        |> .name



-- Part 2


fixUnbalance : Tower -> Tower -> String -> ( String, Int )
fixUnbalance tower cumulativeTower unbalancedNode =
    let
        candidateNodes =
            Dict.get unbalancedNode cumulativeTower
                |> Maybe.withDefault emptyNode
                |> .subnodes
                |> List.map (\n -> Dict.get n cumulativeTower)
                |> List.map (Maybe.withDefault emptyNode)

        groupedNodes =
            candidateNodes
                |> List.Extra.groupWhile (\a b -> a.weight == b.weight)

        errorNode =
            groupedNodes
                |> List.filter (\l -> List.length l == 1)
                |> List.head
                |> Maybe.withDefault []
                |> List.head
                |> Maybe.withDefault emptyNode

        cumulativeWeight =
            errorNode
                |> .weight

        correctCumulativeWeight =
            groupedNodes
                |> List.filter (\l -> List.length l /= 1)
                |> List.head
                |> Maybe.withDefault []
                |> List.head
                |> Maybe.withDefault emptyNode
                |> .weight

        actualWeight =
            Dict.get errorNode.name tower
                |> Maybe.withDefault emptyNode
                |> .weight
    in
        ( errorNode.name, actualWeight + correctCumulativeWeight - cumulativeWeight )



-- Main


input =
    "llyhqfe (21)\nvpbdpfm (74) -> ndegtj, wnwxs\ndosteiu (262) -> vliyv, rfxmk, nulxd, tckql\nleqnli (222) -> wuttw, nckca\ncgztcyz (59) -> zbtmpkc, lleaucw, zxvjkqv, tqjyoj\ndqfti (67)\nvsjhe (34) -> zpbbgqh, menyi, ksasli, uahdbi, ccfiz, kdwmlx\nntzuhe (98)\nmpjrzt (53)\ndnzll (23)\nensyb (18) -> usvzfi, uxxtnll, phrkfo, vntjo\nairqzst (39)\nhfzvg (26)\nwpojcme (79)\nxggisxm (37)\njkqcelt (35)\napjsu (299) -> rgylin, yrmfcs\nodoni (18)\ngzatvf (27)\nazkpaf (81)\ndnyaj (76)\nchfcnsc (70)\nwjdkcjo (29) -> jdntuc, htaxf, edpqtnn\nbejkc (194) -> lqjnh, xkfwmh\nlfapaod (97)\neidqfh (24)\nhaeyms (23) -> akxrge, qgqrmeu, nsnhdll, ydyvay\nialdd (67)\notqufza (116) -> dvasofv, mxdxz\njbopt (91)\nmkxsdn (46)\nvkcim (63)\nypokgio (14)\nwiihwvv (55) -> mivrqpc, hdqgdm, muulq, tveyfha\nrvdldy (47)\nxzsfek (87)\nshkfwm (26) -> yjpzyzx, vdnvw, nsbyncu, wpafb\njdryrup (43)\nzqxhle (53)\nxaaqdv (21) -> kxkwc, mpwnd\nlfmlqs (79)\nmcctaf (37)\nqewiy (18)\nlfzvi (240) -> gxmqlu, sfteyu\nzbtmpkc (94)\njaathmh (33)\nsjwxyqb (55)\nwxvsp (187) -> umiohmp, zeauj\nopghigg (97) -> dletgs, bcgqdc\nkabjov (239) -> eulcspz, nxttce\njivdw (14)\nnckca (6)\nnfeok (203) -> apqan, ywtywz, inoyp\nejyegf (71)\nccmfbok (82) -> jbopt, cteuws, rrsxb, atfjks\nqizkjh (350) -> mxsacj, liiwwfa\ntdfirdf (62)\njwboky (54)\nksnnnc (638) -> iuuzow, eiyvtz, dlxcy, ltfbsgc\nmhbiyxk (15396) -> ehpfjr, zqgeod, hiccoc\nbzenp (37)\nuymhfo (37) -> wiihwvv, jgpsybl, zpxuxph\nvewathl (261) -> bzbxoa, ntzbt, jsizfuj, ikrlxqw\nbvqhn (82)\njqtxjrm (1199) -> aqkclfk, eipaxu, hzvctd, zpohg\nkfuwkh (52)\naamghal (79)\njqywsxa (74)\nehpfjr (42) -> sdxwhvp, itdxbrj, cgztcyz, awylric\ngxmyk (166)\nbeknji (29)\nahvdop (50)\nfpuhllh (8)\ncadtows (49)\nshbrz (874) -> hcywj, pkgyjn, hwxxvlb\ntebvlpn (106) -> kfuwkh, nkuhc\njexcm (33)\ngwplv (33)\nnsckvp (49)\nghaxmrh (10881) -> hhosv, ximzx, ztphu\ngkwamq (54)\nvonve (204) -> scxkq, ubsbx\nhmlil (63)\nrpmzw (97)\nfzkqz (75) -> xxyjm, yjqgw, ejyegf\ndfwamci (36)\nbzbxoa (42)\nqlmbqwi (58)\nivygtzl (1708) -> vjfsl, xfvhi, sbhfnav\nvliyv (20)\nmecsrr (81)\nuskdpcu (388) -> nktkgz, xcuud\nvwktc (60)\nfkpjukc (42)\nnsnhdll (70)\ndpgggti (8)\nnxmxgax (70)\nvwntogi (64)\njshekxk (42)\nncxhv (184) -> ddxiiha, hcvuc, tebvlpn, kkjen, wjkalv\nurpzfa (58)\nocrgjl (2738) -> qogmb, qxirdyg, aovhss\ngtxvgr (58)\npsqgnhx (20)\nvkoor (69)\nmzpeoz (50)\nsbebrkf (12)\nzupsoqc (20)\neiyvtz (65) -> modakko, mlydcn\njdvuj (8)\nbytizsx (61)\ndhamym (17)\nzldebh (76)\nesmltj (21)\ntihzzf (701) -> wyeoaxt, hrkhlaq, vyccl, jezmn, nmmrik\nrmriv (27)\nbyiqom (88)\nmnkamc (1717) -> idhjov, pyurvrc, ahpitb\nfvtofr (44) -> jexcm, jokgw, slmnzei\nppkpq (41)\nkxkwc (57)\nepnvhbn (21)\ntusmlk (295) -> ialdd, kipiwwk\nlwqscns (14) -> zgyryw, oiooued\nzbmsz (35)\nczmmh (44)\nfmwid (1567) -> dhbxpw, xkzrkzh, wxvsp, zqyrggw\nsviwi (15)\nnodqkan (89)\nqddbmn (72)\nkiphte (353) -> kfiggar, rncuf\njyajecr (14)\nclqwflm (17)\nkkjen (36) -> rftaqhw, hxtejel\niokwnq (25)\nyelgho (38) -> uiagqs, dzrflyr, tdfirdf\nrgocso (41)\nmdsywgy (70)\nknhvwhl (298)\nzpxuxph (399)\ndhbxpw (19) -> itfnye, yghucrl, ekvkidl\nrxeqfsj (24)\nsjzapjt (85) -> opndzmu, ilexb, tqddro\nnbybi (7) -> vwntogi, mhvzqc\nbnkbyp (73) -> hagkc, arfsqdz, wbzmjq, eisjz\njezmn (40) -> nsmlghl, lakhmm\niwlxpz (18)\nvjfsl (87) -> gccvp, wkble, ilshxl, jqywsxa\nztphu (983) -> picliob, wcblyq, ollvgn\neuwfw (12)\nrccvm (363) -> mutyu, kqltwau\nhifms (54)\nbyldgs (79)\nforycux (37)\nxwwjzx (39)\nqvqzuic (6)\nuylrp (7179) -> apqwz, nsqaxp, yffumkx\njblzpyq (45)\npeexz (96)\nldcaht (98)\nwwggl (157) -> ssxpawm, brjzpkm, woionr\nyyhkwha (179) -> yiehfd, jkqcelt, fuvikt\nrjtdc (44)\nwnwxs (83)\nmsigvaq (96)\nojrggba (69)\npvctv (3341) -> gkwamq, sattu\nctrdahm (24)\nxfvhi (283) -> zkphtd, qmncedz, lsdkm, iokwnq\ngkrtbv (30)\naovhss (185) -> wevhizp, lmnews\ntfpbait (45)\nmkeen (102) -> jttgtsg, phkcge, zxyrq, telnuq\nefbrhl (31)\nwjkalv (54) -> eloku, xwwjzx, etyja, vghvcv\nndois (317) -> csuoxe, jwboky\nvfpwu (712) -> tusmlk, vewathl, jxfbflh, lcefyg, bnkbyp\nzdkgm (13) -> xpkyf, eaqhut\nrhpxizt (449) -> lcnqmai, cpjkn, ccfbpoc\ndmhfz (66) -> ojrui, bkuazfi, yedrd\nbpbwn (97)\ncdglv (1515) -> pbimnll, rcyjnsi, sfnsx, hfdoqqt\nkfcaozk (27)\nlqjnh (38)\nqhjui (47)\nntzbt (42)\natfjks (91)\nkfiggar (24)\nqeoyu (42) -> liukun, tdvorom, knhvwhl, ombds\ntlnuq (76)\nzpedug (76)\narrok (230) -> ypokgio, qonkb\nwvvmksv (21)\nscxkq (33)\niuuzow (117) -> unlwjj, ayfcyul\nbkuazfi (55)\nhrkhlaq (66) -> mwavu, jbtqs\nnbtsze (80)\ndarmn (96)\nkaugsh (378) -> fhzkakn, epdzg, ogsxfk, rzoojpm, dabvuui\nelhxdco (220) -> wmmrhf, tcxkqku\nmgnux (46)\nyzhwurz (136) -> pksyw, osrkwa\nmuulq (86)\nayfcyul (43)\nuryery (39)\nxipivez (33)\nubsbx (33)\ntcxkqku (21)\ndhqjni (17)\nnokkziw (73)\nyzjiby (79) -> numbey, jqtxjrm, ybkdekt, ciwpis, smkqg\nybjghed (91)\naxleb (81)\njfoztzy (37)\nrjoszhu (92)\nvimazqc (93)\nsvhcnju (24)\nmwssex (55)\nwfmmajk (177) -> kfcaozk, pmfbr\nhxjopp (228) -> nsckvp, cadtows\nbieswf (51)\ndabvuui (38) -> nxmxgax, xmtosc, chfcnsc\napqan (38)\nlivac (240)\nkcotwhf (1006) -> jbztwms, pfpmube, bgeec, hhawhzk\nmxdxz (44)\nhhosv (40) -> klnemf, vrzsj, bacazl, gzepcax, onqop, afkeosv, zvlafea\neytppcy (309) -> ltifq, ehxjsgn\nkyzjusc (37)\ngmsmnlz (36)\nmncyztp (1990) -> fsmzfjp, kndrzyc, svvirl\nbnkfzle (261) -> bvqhn, edihrrv\njakfuqo (317) -> sxfxnp, tdrdp\nuxqiqg (95) -> xwyggz, oxtvu, zwtaqj, cxvse\ntftwygl (29)\nytaus (47)\nuxxtnll (81)\nyghucrl (66)\ntnxoqxw (93)\nemkyoy (354) -> rddeecm, dugvnav\nhmorsv (64)\njwidjq (217) -> qfyor, bpsyylv\nkeily (231) -> ueywo, xlpqnhm\numiohmp (15)\nkiuayw (24)\nenuzo (63)\nnoejr (83)\ntveyfha (86)\nohmvcr (98)\nxfzxw (67)\nmodakko (69)\ncwemvgf (85) -> iebsger, mtoqh, ciabx, puzwwgx\nwmmrhf (21)\nvxghl (48) -> umgqr, cbvwcv\ncnvghq (33) -> byiqom, ehljn\niemkgdl (79)\ngpucfv (270) -> bscob, leyohju\nmlydcn (69)\ngsgexgb (90) -> cwemvgf, cukbzsw, iaiqz, gveadp, rccvm\niybrmf (27)\nfrruz (7530) -> pkrxt, ifwkgxo, abpry, dydso, fjjuj, sgfbfq\nxxvlxs (37)\nuqttm (33)\neryxwj (90) -> vxtwg, bmtjkw\nbsdxw (29)\nffxkad (91)\nciwpis (1215) -> elhxdco, ccmod, etuteik, jaxkva\nzzxzeuo (62)\nqycoh (66)\nsuprw (34) -> vpbdpfm, kacamw, dwdczlx, mrqaryt, rjnzfa\nefpvvp (7686) -> mncyztp, tleviy, uuftjqx, vsjhe, mmutg, wzvjkiu\nbpsyylv (17)\nthmnm (80)\nrvpbx (256) -> zwxlf, ojrggba\nndegtj (83)\nksasli (331) -> cyxtnfe, xrtkqi\nvyfbsgv (81) -> rwxfhk, kihifp, ndois, kupmpp, eytppcy, bnkfzle, fycnyn\nkjyufi (37)\nykxkv (299) -> ijzgy, dsbxavd\nwnfcsap (32)\nicoti (69)\nakowch (67)\nxlpqnhm (59)\nyzrfzv (73)\njmmbca (91)\nmfacoz (23)\nbcgqdc (77)\nohbfa (79)\nnumbey (1861) -> lwqscns, arlrk, klqvgm\nkfgyus (42)\ndlfay (16) -> zksnaz, miocbjk\ndwdczlx (162) -> cmdcov, pbhsevc\nliiwwfa (22)\nikrlxqw (42)\nfwbang (99684) -> lvxnl, aspplbw, uylrp, yzjiby\nryzfgj (44) -> lrbozkj, mpjrzt\nzrjtxfa (9314) -> iixjr, vfpwu, ivygtzl, vgwfukr\nmkxatmb (91)\nzxmsme (66) -> lgjbhwy, jugycbw, dnzll\nhxtejel (87)\nrsblo (385)\nrqbgxlt (24)\nrbbhhe (91)\nkcbag (36)\nfdorzyx (49)\nlcnqmai (12)\ndvpmg (1474) -> uagszs, otqufza, mjmpkq\nlakhmm (99)\nzxyrq (18)\nfthgkl (55) -> ekuibos, zmtszz, peexz\nonqop (60) -> bnryi, kjyufi, kyzjusc, elukq\ntismupk (86)\nhhrqbn (75)\ncycky (75)\nxksjes (103)\nufhjnc (106) -> rbbhhe, ybjghed\naqkclfk (88) -> nodqkan, tahov\nfuehgn (175) -> dnliq, dhqjni, ruszodn, cjagg\nafkeosv (181) -> qfwtxzq, aowuj, trkvrk\nbrdkwc (45)\nyedrd (55)\nwyeoaxt (104) -> inlzx, akowch\nowfrl (1516) -> rsblo, hfytix, ggwwhvf, ykxkv\ngosjs (24)\nmxsacj (22)\nosrkwa (32)\ndrffb (15)\nzsucroj (76)\nermgcbt (78)\njpjehc (37)\nvdnvw (58)\nholcy (66) -> frruz, hbzxaji, mhbiyxk, zrjtxfa, efpvvp, acmrndk, trrkrqa\ngjbijgl (64)\nlphqgek (21) -> qdpnoic, qvjiwvb, qxoly\nuhsdpj (72) -> rslnx, bmaoav\nakxrge (70)\nyerckb (255) -> dpgggti, ssysjwe\neipaxu (150) -> qlmbqwi, fsuglk\npbhsevc (39)\npfmordc (36)\nrdjfp (1393) -> dmhfz, wfmmajk, utoogeb\ntpbbd (91)\ndexwo (223) -> fqjdoe, ovpyq\nhwinqpr (27)\nmqgmc (150) -> vkoor, hvdwvo\nqrhweil (99)\nkrdsv (24)\ndydso (1002) -> evbilqr, glbaxl, yzhwurz, usubx, uxxyr, gremk\nwowirye (47)\neaqhut (70)\nilshxl (74)\nyqsfolo (11) -> vrdrc, utqxez\nqniem (85) -> qdnuduv, zsseyik, xxardqs, xafip, uhsdpj, xxehapc, pmwosk\nojbyg (21)\nvkwcj (96)\nrtvpznv (257) -> emxviup, dfwamci, pfmordc, ikcjmxi\nabpry (1005) -> uxqiqg, adxplm, xlbjv\nnhtetdw (29)\nzstbuv (60)\nzmtszz (96)\nnfccf (26) -> zpqpd, skbxo, hzkvyoj, fhivbs\nkrkeek (52)\nowgbqb (30)\ntekug (77)\njaxkva (138) -> zzxzeuo, npxeql\nhqqxg (43)\nungfmbw (61)\nedpqtnn (96)\nydyvay (70)\nniopwq (42)\njgpsybl (317) -> ppkpq, fezoee\noyypq (142) -> iwxgwc, qyovvxb\ninlzx (67)\nedihrrv (82)\nqsloy (44)\nyffumkx (9) -> jakfuqo, ouxsgm, keily, pshyy\nfhivbs (92)\nfkprhv (31)\nrhgyz (38)\neiyxgk (76)\npzemz (76)\nlcefyg (353) -> twvjddq, rhgyz\nvrpyfgm (88)\nhfytix (81) -> cjctf, fckcu, ztcqm, pzemz\nvoiqnou (147) -> czmmh, rjtdc\nozwdh (96)\nekhsrgq (25)\nxmtosc (70)\nyfrewb (77)\nqfyor (17)\nijuod (93) -> ldcaht, ruuhrmf, dfbabey, bbdfr\npknpuej (91) -> livac, vxghl, qcccxc, mxprsl\ngwvsbo (76)\nfqjdoe (39)\ngqahoa (57)\nvykav (89) -> dfhtf, yopex, ypzxdhs\npicliob (97) -> bzenp, jfoztzy\nfhzkakn (107) -> wowirye, aseilg, jmutqq\numgqr (96)\nqmlguo (96)\ngosak (27)\nnvatz (63)\nzsgnve (39)\nfyvjfxi (58)\noxtvu (76)\nhoewyjx (47)\nqykdedu (63)\ntocrk (88) -> nzyls, qlgljuh\nssnoqt (24)\nmhvzqc (64)\narfsqdz (89)\nvrzsj (127) -> tetfdv, ornacig, yrdbx\ntszune (40)\njcuhfsd (80) -> sordz, zmfhyr\ndqaov (96)\njeafpic (32)\ntdvorom (246) -> hfzvg, dapey\ndldcoc (149) -> gwvsbo, eadjn\nngxtfx (91)\noiooued (60)\nnmhmw (979) -> krdsv, kiuayw, rovftl\nyqmbbyr (24)\nzvlafea (140) -> wtjoxu, dxszgsr\nuiagqs (62)\nlhpjahj (86)\nuagszs (36) -> cflribm, dsukkg\numqlwls (202)\npazby (45)\nmqayze (55)\nnzeqmqi (12216) -> nmhmw, pknpuej, rfkvap\nhfdoqqt (128) -> sevcqp, bieswf\nejmfnnu (43)\nhupmm (76)\nwnahs (18)\noomve (87)\nhdfsofm (75)\nqonkb (14)\ncnlny (278)\nogsxfk (140) -> iybrmf, xixiloi, hwinqpr, jdmrbxc\nnafdo (23)\njsizfuj (42)\nmwavu (86)\nciabx (96)\nljwcd (16) -> nokkziw, cjgpfb, yzrfzv\netyja (39)\nhcrzxz (78)\nxwyyfr (256) -> jyajecr, jivdw\nlrbozkj (53)\noibnbf (15)\nbbdfr (98)\nzwzgp (14)\nsvvirl (62)\ntlkrx (23)\nkndrzyc (62)\neuenhl (53)\nipvrlll (86)\nggwwhvf (160) -> hhrqbn, zvazn, hdfsofm\nsofrg (281)\njyovf (29)\nagobkww (69)\ngzepcax (50) -> wpojcme, pprspr\ncxvse (76)\nrftaqhw (87)\nyzbmyaw (86)\njjbmtij (54)\nwhuozum (35) -> suftfkn, gtxvgr, igxdio\njxfbflh (229) -> xkyocjn, ahvdop, ewlsf, jejwwxj\nwoionr (15)\npkgyjn (76) -> urpzfa, fyvjfxi\npwydnik (63)\nchhli (46)\ngdvcou (189) -> jdglmn, mgnux\ncbvwcv (96)\ngtiqar (29)\ntxcwm (29)\nfvojv (116) -> rycpngd, hifms\ndzxjy (96)\nniznnko (44)\njmutqq (47)\nsevcqp (51)\npmfbr (27)\nshoxg (7)\nltfbsgc (109) -> rvdldy, qhjui\nltifq (58)\njxzyg (37)\njttgtsg (18)\nzwtaqj (76)\nmwussz (143) -> etotvx, lbmvl, xlavrvm, rmriv\nzwlok (143) -> zvtgd, shoxg\nhcywj (102) -> brdkwc, tfpbait\nkbuslbp (81)\nsmunvi (172) -> zqxhle, euenhl\nqlgljuh (26)\ngwcqtcr (73)\nkacamw (170) -> bexrple, wetutqh\ndvkbqm (20)\neaerpmi (1342) -> rjoszhu, migwxez, izydgv\nealilsq (50)\neisjz (89)\nrihil (12)\nysabu (24)\nombds (178) -> wlrihpy, vwktc\nsueftvh (81)\nmrqaryt (80) -> thmnm, kligtj\nxkyocjn (50)\nprywl (43)\nfonky (47)\nbhddwe (64)\nmtoqh (96)\npksyw (32)\njndnfa (45)\nguqul (222) -> oonfc, irpjsbf\nnkuhc (52)\napqwz (934) -> gjvcdp, fnuzrye, zwlok\nojrui (55)\ntckql (20)\numsilqj (32)\nxswwe (212) -> ffxkad, ngxtfx\nhwtztim (187) -> idaqt, jshekxk\nnuzxo (83)\nitfnye (66)\nqcedbm (2484) -> bkipqaq, xmcqygt, fvtofr, zjksxbk\njutbah (55)\nscntyh (5)\nliukun (70) -> oftcgd, eiyxgk, sqbfin\nvqxwlkh (8119) -> kcotwhf, ksnnnc, shbrz, jtxdihn, yixpr\nywqtog (136) -> epelgzz, lpvwee\noftcgd (76)\nrdzvcb (184) -> clqwflm, dhamym, qkmkjm\ntqjyoj (94)\npprdw (42) -> tpbbd, mkxatmb\nbgmypwk (22)\nhznriv (96)\ntleviy (973) -> rtvpznv, gbpxwcx, kiphte\ncsuoxe (54)\npqqcnkr (39)\nhcqrrju (100) -> xfzxw, zwyhf\namccpoz (254) -> zsaen, jfkvg\nwbzmjq (89)\nxpkyf (70)\nhnofc (85)\ndsukkg (84)\nqfifp (6)\nqmncedz (25)\ndyrik (37)\niaiqz (445) -> sgjywom, alwbi\nxpjzc (86) -> qrhweil, vsgqkho\nagagr (81)\nfpkktd (25)\nmjmpkq (44) -> nbtsze, hwlgay\nkwhtsv (26)\nawljibm (5)\nhssykro (81)\nvyccl (80) -> aamghal, tkwmbxl\nnelgvnr (2052) -> jwidjq, mwussz, nkuwwiy, opghigg\nzbhioc (20)\nzqgeod (1323) -> sbnod, rtxzoap, zdkgm\nrgqjtw (243)\nvaxouij (343)\njsrpud (57)\nwetutqh (35)\nlsdkm (25)\nsdttg (27)\narlrk (40) -> xlsmzu, hoewyjx\nybkdekt (1234) -> vaxouij, fthgkl, kabjov\nsebno (279) -> qfifp, ghxvqb, qvqzuic, wfazzy\nkeidsd (73) -> hqqxg, ejmfnnu, prywl\nuevcnw (39)\nzvazn (75)\nxdjola (63)\nmiocbjk (75)\nghxvqb (6)\nywtywz (38)\nvtpoo (89) -> vskibye, nelgvnr, qcedbm, owfrl, vyfbsgv\njlfukd (57) -> tnxoqxw, vimazqc\nvkxyhk (41)\ndlxcy (203)\nkosbvn (19)\nwfazzy (6)\nuufonho (46)\nkdwmlx (357)\nlokmiua (2132) -> hdrab, eftrvo, hbnnhyi, avnxndg, tihzzf, nbvtfz, qniem\ndugvnav (46)\nxxardqs (216) -> llyhqfe, ojbyg\nwkble (74)\nlpvwee (67)\npkrxt (1524) -> drwpdaj, ojcinc, hqetmky\nzjksxbk (143)\niebsger (96)\nofwijoe (60)\nqyovvxb (41)\nigxdio (58)\nxkzrkzh (148) -> nafdo, tlkrx, gclbhxw\nruszodn (17)\ndnliq (17)\nqcccxc (114) -> ibiuha, byykf\nbscob (28)\nmivrqpc (86)\nyiehfd (35)\nhbnnhyi (1055) -> ptnjpp, hqcxvkr, aqlvute, yqsfolo\njpexkf (86)\nhagkc (89)\nhlscl (214) -> jblzpyq, jndnfa, twvfw, pazby\nirrca (56)\nsgjywom (12)\nepdzg (248)\nyixpr (1141) -> pjlhta, xksjes, icfxlu\nyjtsmy (85)\ninwmb (53046) -> ghaxmrh, vqxwlkh, nzeqmqi, lokmiua, znypga, vtpoo\netuteik (262)\nwbtqez (87)\nlmnews (26)\nzoovdc (75)\nsmkqg (44) -> vyzukfk, nfeok, apjsu, vbjlbhq, wjdkcjo, ztstgc, olvxzb\ntelnuq (18)\njdmrbxc (27)\nrrsxb (91)\ntetfdv (27)\ncpjkn (12)\ngveadp (429) -> zbhioc, psqgnhx\nzxvjkqv (94)\nmmvszx (12)\nxkfwmh (38)\nvyzukfk (255) -> efbrhl, tqdfypr\ngbpxwcx (77) -> azkpaf, axleb, ngwafk, sueftvh\nblcnplx (32)\ntrkvrk (9)\nnxttce (52)\nrzoojpm (234) -> kbguoiu, yvjjec\nidhjov (49) -> mcctaf, jpjehc\nctnucjw (105) -> ohmvcr, ntzuhe\ntdrdp (16)\nqkmkjm (17)\nbacazl (82) -> enuzo, hmlil\nlgjbhwy (23)\netotvx (27)\npfkbcg (228) -> xkvtxav, oibnbf\nfjlqp (30)\nnsbyncu (58)\nxnackkp (33)\nsxfxnp (16)\nhhawhzk (89) -> dserbhu, ckfagrp\ndsbxavd (43)\nmtbszl (12)\nbkipqaq (71) -> ctrdahm, sptjz, svhcnju\nsndkiv (73)\nidaqt (42)\nsxcfr (98) -> ungfmbw, ynjccf, bytizsx\ngtutcoq (154) -> zqmizps, vxdcv, nmshjlp\nefxxl (69)\nnpxeql (62)\nljelubc (437) -> gwxgtm, umfqiru\nhzbtbe (46)\nbjvncf (49)\nhwxxvlb (48) -> qddbmn, vomiow\nsfteyu (19)\nsbhfnav (255) -> jeafpic, ywecj, wnfcsap, cvgzkp\nbmaoav (93)\nfnuzrye (125) -> vlqcuq, jdvuj, mykvbt, fpuhllh\nvsgqkho (99)\nvkbgz (63)\nrcyjnsi (206) -> rihil, usfvqn\nhdqgdm (86)\nunaqbx (23)\numyrtu (33)\nsordz (47)\nbyykf (63)\nqggexrc (33)\nrovftl (24)\nqdnuduv (120) -> icoti, efxxl\nqogmb (81) -> rnyqcj, hcrzxz\ndfbabey (98)\nvdmkp (38)\nkacpu (90) -> hrbfs, vdmkp\njcegjy (92) -> xoaxcg, mecsrr\nxrtkqi (13)\ncosllh (38) -> hzfwp, gjbijgl\njfkvg (15)\nifwkgxo (1752) -> pbrcoyl, ryzfgj, luswq\ngrcsr (64)\ndzrflyr (62)\nhjtwqe (748) -> mkeen, zowbv, lphqgek, jcuhfsd, qymfgaf\nyjqgw (71)\nidiorl (24)\nusvzfi (81)\nacmrndk (13437) -> cdglv, gsgexgb, fmwid\nvijilqr (271)\ndxszgsr (34)\nehljn (88)\nyutfxcu (72) -> wobno, mwmfw\nngwafk (81)\neijlg (51) -> dzxjy, msigvaq\npkchh (24)\nizydgv (56) -> odoni, wnahs\nwobno (93)\nidfxtqr (96)\nhqetmky (127) -> gwplv, umyrtu, uqttm\nfvjrau (166) -> qsloy, niznnko\nehxjsgn (58)\nhqcxvkr (37) -> oeyrk, ipvrlll\nvaztjkc (75)\nynnfzdz (54)\nvrdrc (99)\nhdrtnjm (273) -> drffb, sviwi\nywecj (32)\niixjr (99) -> hlscl, xswwe, gtutcoq, qizkjh, nfccf, rvpbx, extmwcb\nfuvikt (35)\nkipiwwk (67)\nlakzkpk (84) -> rpmzw, lfapaod\nextmwcb (142) -> vkcim, ihramd, kwkdq, hjwjw\npmwosk (118) -> leefit, mdsywgy\nruuhrmf (98)\ntqdfypr (31)\nitdxbrj (366) -> fjebkm, mfacoz, unaqbx\nkfgmuj (54) -> ljelubc, jlewu, rhpxizt, pjujpa, ijuod, xauyij, bbrdet\njtxdihn (45) -> sofrg, vgdtk, sxcfr, rmivzpg, gdvcou\nyzulmo (33)\nuuamcdc (1954) -> qycoh, jhbov\nochjr (68) -> xhujxe, fonky, olyohzo\nnbvtfz (807) -> hwtztim, yerckb, nyszg, vijilqr\nimtvzmm (40)\ntshcqcu (33)\novsgve (83)\nueywo (59)\ngxmqlu (19)\ntahov (89)\nelukq (37)\nzadsb (87)\nbgeec (53) -> tftwygl, txcwm\nyjpzyzx (58)\nxlbjv (288) -> jxzyg, dyrik, cdanu\nzsaen (15)\nqywtwbt (124) -> esmltj, qsppfv\ndwbirs (5)\noonfc (16)\npyurvrc (49) -> xwidhe, xxvlxs\nzksnaz (75)\nxhujxe (47)\nwctphrv (19)\notadcxu (2028) -> bsdxw, beknji\nhzkvyoj (92)\nzeauj (15)\nzayxe (2822) -> ochjr, cnvghq, whuozum\naqlvute (27) -> absogx, jmmbca\nilexb (72)\nvrdtrmn (69)\ndfhtf (79)\nsbnod (153)\nlwyirb (77)\nhwlgay (80)\nsfnapsi (55)\ncflribm (84)\ncjgpfb (73)\nmenyi (185) -> lhpjahj, yzbmyaw\nubkmjag (78)\nfycnyn (313) -> lwuvg, irrca\nrzixiwv (10)\nqtvcuqk (85)\nalwbi (12)\nsqbfin (76)\nhcvuc (78) -> yzulmo, xnackkp, qggexrc, wjdhaf\nccmod (13) -> ovsgve, noejr, nuzxo\nrslnx (93)\nopndzmu (72)\nmxprsl (84) -> uivazm, ennkek, krkeek\nzowbv (28) -> sndkiv, gwcqtcr\npjujpa (433) -> rgyaijv, kwhtsv\nrnyqcj (78)\nzwxlf (69)\nccfiz (231) -> pwydnik, vkbgz\nzqyrggw (61) -> qqbgbeo, ubkmjag\npuzwwgx (96)\nxwidhe (37)\nasmikyo (24)\nhdrab (1083) -> umqlwls, tuldcdj, keidsd, wwggl\nximzx (146) -> ywqtog, bejkc, vonve, wmdgia, ggeae\nhzfwp (64)\nwmdgia (14) -> bhddwe, zrzgp, hmorsv, grcsr\npfpmube (73) -> wctphrv, kosbvn\neadjn (76)\ntxplq (24)\nqsppfv (21)\nqxoly (51)\nsardhwu (77) -> iemkgdl, byldgs\nvlqcuq (8)\neloku (39)\nypzxdhs (79)\nfsmzfjp (62)\nzqmizps (80)\nqqpnt (1108) -> hxjopp, gpucfv, vykav\naowuj (9)\nxlsmzu (47)\njokgw (33)\nepelgzz (67)\ncjctf (76)\nqgqrmeu (70)\neulcspz (52)\nsattu (54)\nvgdtk (101) -> ggffqux, jlinuge, hvinb\nbrjzpkm (15)\nyvlwtb (114) -> wbtqez, zadsb\nssysjwe (8)\nptnjpp (135) -> forycux, xggisxm\nadxplm (295) -> nfxyjl, lksgoz\nqvjiwvb (51)\nlleaucw (94)\nabsogx (91)\nzflsryn (239) -> fkprhv, blloue\ninoyp (38)\nluswq (102) -> yhlyk, mmvszx, euwfw, sbebrkf\nrncuf (24)\njdntuc (96)\ngremk (86) -> gqahoa, jsrpud\nmutyu (53)\nyhlyk (12)\newlsf (50)\nxixiloi (27)\nwoiwqf (5)\nfckcu (76)\nddxiiha (16) -> bpbwn, uuxhyt\nnmmrik (73) -> jutbah, sjwxyqb, mqayze\nssxpawm (15)\nrksykyt (77) -> ohbfa, lfmlqs\nzwyhf (67)\njhbov (66)\ntqddro (72)\naafpxpx (30)\nzsseyik (248) -> awljibm, woiwqf\nvxdcv (80)\novpyq (39)\nznypga (14031) -> emkyoy, uskdpcu, ccmfbok\ngwxgtm (24)\nztstgc (125) -> jqwbc, darmn\nusubx (100) -> mzpeoz, ealilsq\nklqvgm (56) -> zsgnve, pqqcnkr\nuuxhyt (97)\nyjxneui (76)\nawylric (393) -> wvvmksv, epnvhbn\nwtjoxu (34)\nztcqm (76)\nwjdhaf (33)\nhrase (25)\nllcpsj (178) -> mwssex, sfnapsi\nhrbfs (38)\nvomiow (72)\nuumrue (29)\nxxyjm (71)\nyrdbx (27)\ndserbhu (11)\ntrrkrqa (48) -> awytebt, ocrgjl, zayxe, pvctv, sdovaq, kfgmuj\nxoaxcg (81)\nsdxwhvp (192) -> agagr, hssykro, kbuslbp\nbeqez (142) -> mtbszl, dytsvc\nkihifp (117) -> tekug, lsxwznl, lwyirb, yfrewb\nfwlyuh (70) -> zbmsz, btldlkh\ncvgzkp (32)\nmigwxez (72) -> rzixiwv, bwekmvc\njqwbc (96)\nihramd (63)\nbwekmvc (10)\noeyrk (86)\nccfbpoc (12)\nbexrple (35)\nrtxzoap (153)\nbvwnlaw (88)\npbimnll (136) -> pkbitw, ytaus\nzciuy (63)\nzvtgd (7)\nuuftjqx (1414) -> guqul, jcegjy, fvjrau\nqymfgaf (66) -> gzatvf, sdttg, gosak, pcacjm\nesbnpk (43)\nekuibos (96)\nazyccec (67)\nvghvcv (39)\nicfxlu (31) -> asmikyo, gosjs, fafrerl\ncmdcov (39)\nrjnzfa (192) -> eidqfh, yqmbbyr\nqxirdyg (121) -> mwhopi, jyovf, nhtetdw, tmvjt\nwlrihpy (60)\nnyszg (95) -> bvwnlaw, vrpyfgm\nhiccoc (1118) -> osbsdhc, dlfay, fsomlm, cosllh\nixiqnn (138) -> vthnh, cycky\nuahdbi (313) -> bgmypwk, rzxyny\nahpitb (25) -> yyoptv, qihhif\nzrzgp (64)\nxxehapc (120) -> vrdtrmn, agobkww\nkwkdq (63)\nprhgge (49)\nornacig (27)\nwuttw (6)\nsjbalvv (186) -> uufonho, chhli\nxauyij (427) -> uumrue, gtiqar\nthahonu (208) -> eacnma, fpkktd\nvgwfukr (1921) -> hcqrrju, ckqwb, leqnli, xavfse\npshyy (97) -> qykdedu, zciuy, nvatz, xdjola\ntkwmbxl (79)\nphkcge (18)\nszrnpdw (27)\nfafrerl (24)\naryqw (118) -> ysabu, pkchh\ntwvfw (45)\nyopex (79)\nsptjz (24)\nmwmfw (93)\nsuftfkn (58)\nfjjuj (1782) -> fwlyuh, tocrk, eryxwj\njbztwms (111)\nollvgn (15) -> ermgcbt, kjikhxm\npcacjm (27)\nnldrlb (55)\nyyoptv (49)\nqfwtxzq (9)\nxavfse (42) -> hznriv, vkwcj\npbrcoyl (40) -> nldrlb, wnhseb\ndytsvc (12)\nwpafb (58)\naspplbw (4922) -> eaerpmi, hjtwqe, fikvmjg, kaugsh\nmpwnd (57)\nxmcqygt (65) -> uryery, tlkive\nwevkksz (49)\nikcjmxi (36)\nojcinc (106) -> fjlqp, owgbqb, aafpxpx, gkrtbv\njbtqs (86)\nlksgoz (52)\ntntqpl (202) -> jdryrup, esbnpk\nzpbbgqh (53) -> zpedug, hupmm, yjxneui, zldebh\nrgylin (9)\njdglmn (46)\npkbitw (47)\nrfxmk (20)\nmykvbt (8)\ntwvjddq (38)\nslmnzei (33)\nzpqpd (92)\nlvxnl (3990) -> qeoyu, uymhfo, suprw, ncxhv, drrbwlp, kzwamsk\nfikvmjg (70) -> arrok, thahonu, pfkbcg, yutfxcu, shkfwm, clsve\nekvkidl (66)\niwxgwc (41)\ncbwsr (55)\nkqltwau (53)\njlewu (320) -> zphlpeu, kcxfwz, cbwsr\nbnryi (37)\ndletgs (77)\njlinuge (60)\nennkek (52)\nrzxyny (22)\nuivazm (52)\nhgoesez (214) -> umsilqj, blcnplx\nxkvtxav (15)\nclsve (138) -> ofwijoe, zstbuv\nleefit (70)\nymwqj (301)\nklnemf (76) -> xipivez, tshcqcu, jaathmh, vpzylgj\nqdpnoic (51)\nfsuglk (58)\ndvasofv (44)\ncjagg (17)\nkcxfwz (55)\nkjikhxm (78)\nnulxd (20)\nzgyryw (60)\nnktkgz (29)\npprspr (79)\nwzvjkiu (2006) -> yjtsmy, gjpjta\nusfvqn (12)\nutoogeb (183) -> idiorl, txplq\ngclbhxw (23)\nfsomlm (74) -> hzbtbe, mkxsdn\npxjgtg (219) -> uevcnw, airqzst\nnfxyjl (52)\nhtaxf (96)\nouxsgm (241) -> ynnfzdz, jjbmtij\ncdanu (37)\nzpohg (230) -> qewiy, iwlxpz\nnmshjlp (80)\nuxxyr (26) -> oomve, xzsfek\nwcblyq (171)\nqqbgbeo (78)\nibiuha (63)\nippnuw (342)\njusoe (27)\neftrvo (919) -> rgqjtw, fuehgn, eijlg, jlfukd\nrgyaijv (26)\nfezoee (41)\nzmfhyr (47)\nmxltn (14)\nrfkvap (655) -> zxmsme, nbybi, xaaqdv\numfqiru (24)\nsyeyppr (20)\ntuldcdj (152) -> ekhsrgq, hrase\nskbxo (92)\nlwuvg (56)\nhzvctd (256) -> dwbirs, scntyh\ncteuws (91)\ngjpjta (85)\nijzgy (43)\nrddeecm (46)\nhvinb (60)\ngsrui (49)\nvxtwg (25)\nvthnh (75)\nolyohzo (47)\nbtldlkh (35)\nlbmvl (27)\ngjvcdp (23) -> azyccec, dqfti\nspnzn (70) -> ixiqnn, llcpsj, ufhjnc, fzkqz, mqgmc, tntqpl, yvlwtb\nfjebkm (23)\nkupmpp (41) -> qmlguo, dqaov, ozwdh, idfxtqr\nawytebt (2553) -> oyypq, pprdw, fvojv, yelgho\nrycpngd (54)\nvpzylgj (33)\nosbsdhc (166)\nbdplsy (20)\nwnhseb (55)\nemxviup (36)\nazqje (72) -> holcy, fwbang, inwmb\nblloue (31)\ngccvp (74)\nsfnsx (83) -> prhgge, bjvncf, wevkksz\nnylej (42)\nzkphtd (25)\ndrwpdaj (58) -> nylej, fkpjukc, niopwq, kfgyus\nolvxzb (89) -> tlnuq, zsucroj, dnyaj\npjlhta (63) -> zupsoqc, dvkbqm\nxafip (176) -> rgocso, vkxyhk\nqihhif (49)\nggffqux (60)\nbbrdet (445) -> bdplsy, syeyppr\nvbjlbhq (219) -> gsrui, fdorzyx\nmwhopi (29)\neacnma (25)\ndrrbwlp (59) -> ljwcd, voiqnou, rdzvcb, sardhwu, rksykyt\ndapey (26)\nrmivzpg (227) -> szrnpdw, jusoe\nckfagrp (11)\nnkuwwiy (79) -> jpexkf, tismupk\njejwwxj (50)\ncukbzsw (441) -> mxltn, zwzgp\neobbt (229) -> rxeqfsj, rqbgxlt, ssnoqt\nggeae (270)\nxdfnmvr (147) -> vaztjkc, zoovdc\nfmcwdv (85)\nyvjjec (7)\nkzwamsk (325) -> sebno, haeyms, hdrtnjm\nhvdwvo (69)\nbmtjkw (25)\nrwxfhk (353) -> gmsmnlz, kcbag\nynjccf (61)\nzxozp (42) -> hnofc, fmcwdv, qtvcuqk\nxcuud (29)\nxwyggz (76)\nglbaxl (120) -> imtvzmm, tszune\nunlwjj (43)\nhjwjw (63)\njugycbw (23)\ncyxtnfe (13)\nphrkfo (81)\nmmutg (1040) -> yyhkwha, xwyyfr, xpjzc, amccpoz\nvntjo (81)\ntlkive (39)\nkbguoiu (7)\nnsqaxp (575) -> kacpu, gxmyk, aryqw, qywtwbt, beqez\ntmvjt (29)\nvskibye (1388) -> sjbalvv, hgoesez, lfzvi, lakzkpk, smunvi, cnlny\nsgfbfq (95) -> eobbt, ymwqj, sjzapjt, dldcoc, dexwo, ctnucjw, zflsryn\naseilg (47)\nhbzxaji (6140) -> otadcxu, dvpmg, qqpnt, mnkamc, spnzn, uuamcdc, rdjfp\nxlavrvm (27)\nwevhizp (26)\nyrmfcs (9)\nnsmlghl (99)\nsdovaq (2558) -> zxozp, pxjgtg, xdfnmvr\nirpjsbf (16)\nevbilqr (200)\navnxndg (865) -> ippnuw, ensyb, dosteiu\nutqxez (99)\nleyohju (28)\nnzyls (26)\nlsxwznl (77)\nzphlpeu (55)\nckqwb (234)\nkligtj (80)"


part1 =
    let
        nodes =
            input
                |> String.lines
                |> List.map parseTowerNode

        ( cumulativeTower, _ ) =
            parseCumulativeTower Dict.empty "" [] nodes
    in
        findRoot cumulativeTower


part2 =
    let
        nodes =
            input
                |> String.lines
                |> List.map parseTowerNode

        tower =
            parseTower Dict.empty nodes

        ( cumulativeTower, unbalancedNode ) =
            parseCumulativeTower Dict.empty "" [] nodes
    in
        fixUnbalance tower cumulativeTower unbalancedNode


main =
    Answer.render part1 part2
