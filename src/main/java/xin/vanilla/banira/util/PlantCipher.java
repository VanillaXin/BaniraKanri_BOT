package xin.vanilla.banira.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

public final class PlantCipher {

    private PlantCipher() {
    }

    private static final String BASE64_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    private static final String SALT = "BaniraKanri";
    private static final List<String> PLANTS = Arrays.asList("矮蒿", "矮桦", "矮韭", "矮蓼", "矮松", "矮桃", "艾堇", "艾麻"
            , "安蕨", "八宝", "八角", "巴豆", "巴柳", "芭蕉", "菝葜", "坝竹", "霸王", "白桉", "白菜", "白草", "白蟾", "白刺", "白杜"
            , "白桦", "白及", "白芥", "白柯", "白兰", "白簕", "白梨", "白栎", "白蔹", "白柳", "白麻", "白茅", "白楠", "白扦", "白前"
            , "白楸", "白薷", "白术", "白树", "白檀", "白藤", "白薇", "白鲜", "白苋", "白英", "白芷", "百部", "百合", "柏木", "摆竹"
            , "败酱", "稗荩", "斑茅", "斑竹", "斑籽", "半夏", "苞藜", "苞茅", "枹栎", "薄竹", "抱草", "杯菊", "杯苋", "北艾", "北葱"
            , "北韭", "荸艾", "鼻花", "笔草", "笔竹", "荜拔", "蓖麻", "弊草", "薜荔", "萹蓄", "扁豆", "扁蕾", "扁桃", "变竹", "杓兰"
            , "藨草", "滨艾", "滨菊", "滨藜", "滨柃", "滨麦", "滨榕", "滨枣", "槟榔", "冰草", "兵豆", "菠菜", "擘蓝", "猜莓", "彩花"
            , "菜豆", "菜蓟", "菜椒", "菜蕨", "菜苔", "蚕豆", "苍耳", "苍术", "糙草", "草果", "草龙", "草莓", "草棉", "侧柏", "侧蒿"
            , "箣柊", "茶梨", "茶藤", "檫木", "柴桂", "柴桦", "柴首", "菖蒲", "肠蕨", "常桉", "巢蕨", "车前", "柽柳", "赪桐", "橙桑"
            , "黐花", "池杉", "赤桉", "赤瓟", "赤才", "赤车", "赤豆", "赤麻", "赤楠", "赤松", "赤竹", "翅柃", "虫豆", "稠李", "臭草"
            , "臭椿", "臭蒿", "臭荠", "臭菘", "臭樱", "雏菊", "吹树", "垂柳", "春兰", "春蓼", "春榆", "莼菜", "慈竹", "刺柏", "刺槐"
            , "刺藜", "刺蓼", "刺莓", "刺芹", "刺楸", "刺桑", "刺桐", "刺苋", "刺榆", "刺芋", "刺榛", "葱草", "葱芥", "楤木", "丛菔"
            , "粗榧", "簇芥", "翠柏", "翠菊", "翠蕨", "翠雀", "寸草", "大桉", "大参", "大豆", "大椴", "大管", "大戟", "大蕉", "大麦"
            , "大薸", "大青", "大黍", "傣柿", "丹参", "丹草", "单竹", "淡竹", "当归", "刀豆", "岛榕", "地蚕", "地肤", "地柑", "地桂"
            , "地果", "地黄", "地椒", "地锦", "地菍", "地笋", "颠茄", "吊兰", "蝶豆", "蝶须", "蝶竹", "丁茜", "冬瓜", "冬红", "冬葵"
            , "冬桃", "董棕", "冻绿", "兜藜", "斗竹", "豆梨", "豆薯", "毒参", "毒豆", "毒瓜", "毒麦", "毒芹", "独活", "独牛", "杜衡"
            , "杜梨", "杜楝", "杜若", "杜松", "杜香", "杜英", "杜仲", "短蒟", "椴树", "莪术", "鄂柃", "鳄梨", "儿茶", "耳草", "耳菊"
            , "耳柳", "发草", "番茄", "番薯", "繁缕", "方榄", "方竹", "防风", "仿栗", "飞廉", "飞蓬", "菲柞", "榧树", "费菜", "粉葛"
            , "粉团", "风兰", "风龙", "风藤", "枫茅", "枫杨", "凤瓜", "凤梨", "凤竹", "佛手", "莩草", "浮萍", "辐花", "福参", "俯竹"
            , "腐草", "馥兰", "甘草", "甘菊", "甘蓝", "甘松", "甘遂", "甘蔗", "柑橘", "橄榄", "刚松", "刚竹", "岗柃", "岗松", "港柯"
            , "杠柳", "高粱", "藁本", "茖葱", "格木", "葛属", "个溥", "珙桐", "贡甲", "钩藤", "钩吻", "狗脊", "枸骨", "枸杞", "构棘"
            , "构树", "古柯", "谷蓼", "谷柳", "谷木", "瓜栗", "栝楼", "拐芹", "冠黍", "贯众", "灌柳", "光蓼", "光柃", "光竹", "桄榔"
            , "蛤兰", "海莲", "海漆", "海桑", "海通", "海桐", "海芋", "寒兰", "寒莓", "寒竹", "蔊菜", "旱稗", "旱禾", "旱蕨", "旱柳"
            , "旱茅", "旱芹", "旱榆", "杭蓟", "蒿蕨", "蒿柳", "貉藻", "诃子", "合欢", "合萌", "河竹", "褐梨", "鹤虱", "黑蒿", "黑桦"
            , "黑荆", "黑柯", "黑柃", "黑麦", "黑桑", "黑柿", "黑蒴", "黑松", "黑杨", "黑榆", "红椿", "红葱", "红瓜", "红桧", "红花"
            , "红桦", "红蕉", "红柯", "红蓼", "红木", "红楠", "红茄", "红桑", "红砂", "红杉", "红柿", "红树", "红松", "红药", "红叶"
            , "红芋", "红锥", "猴樟", "篌竹", "厚朴", "厚藤", "胡椒", "胡桃", "胡杨", "槲蕨", "槲栎", "槲树", "虎刺", "虎掌", "虎杖"
            , "瓠瓜", "花红", "花椒", "花葵", "花锚", "花竹", "黄檗", "黄蝉", "黄槿", "黄荆", "黄精", "黄葵", "黄连", "黄柳", "黄栌"
            , "黄麻", "黄茅", "黄泡", "黄皮", "黄耆", "黄杞", "黄芩", "黄杉", "黄檀", "黄藤", "黄桐", "黄薇", "黄杨", "黄药", "黄樟"
            , "幌菊", "灰蓟", "灰柯", "灰莉", "灰柳", "灰竹", "辉韭", "茴香", "蛔蒿", "蕙兰", "篲竹", "火葱", "火棘", "藿香", "鸡麻"
            , "鸡桑", "姬蕨", "及已", "吉贝", "蒺藜", "蕺菜", "戟蕨", "戟柳", "荠苨", "荠苎", "冀韭", "檵木", "加杨", "嘉兰", "荚蒾"
            , "甲竹", "假稻", "假蒟", "假芋", "坚桦", "樫木", "碱蒿", "碱韭", "碱茅", "碱蓬", "碱菀", "建兰", "茳芏", "姜花", "姜黄"
            , "豇豆", "疆菊", "降香", "蕉麻", "蕉芋", "角蒿", "角花", "角蕨", "角竹", "藠头", "节瓜", "结香", "介蕨", "芥菜", "芥蓝"
            , "金草", "金豆", "金柑", "金瓜", "金橘", "金兰", "金茅", "金松", "金英", "金竹", "筋藤", "堇菜", "锦葵", "锦竹", "荩草"
            , "荆豆", "荆芥", "荆条", "粳稻", "九节", "韭葱", "酒椰", "桔梗", "菊蒿", "菊花", "菊苣", "菊芋", "橘草", "咀签", "蒟子"
            , "榉树", "巨柏", "巨杉", "锯蕨", "卷柏", "卷丹", "卷耳", "绢柳", "决明", "蕨麻", "蕨萁", "爵床", "铠兰", "糠稷", "糠藤"
            , "扛竹", "榼藤", "可可", "空竹", "扣树", "苦参", "苦茶", "苦刺", "苦葛", "苦瓜", "苦芥", "苦绳", "苦树", "苦杨", "苦蘵"
            , "苦槠", "苦竹", "苦梓", "块蓟", "筐柳", "魁蒿", "魁蓟", "剌瓜", "蜡菊", "辣根", "辣椒", "辣木", "辣莸", "来檬", "梾木"
            , "赖草", "蓝桉", "蓝蓟", "蓝树", "榄李", "狼毒", "榔榆", "簕竹", "雷楝", "蕾芬", "类芦", "冷蒿", "冷蕨", "冷杉", "狸藻"
            , "黎檬", "黎竹", "藜芦", "黧豆", "李榄", "里白", "鳢肠", "丽豆", "丽蓼", "丽薇", "荔枝", "栗蕨", "连翘", "莲桂", "亮蒿"
            , "辽椴", "辽杨", "蓼蓝", "列当", "林蓟", "林柳", "橉木", "苓菊", "柃木", "铃兰", "凌霄", "榴莲", "瘤菅", "瘤蕨", "柳兰"
            , "柳杉", "龙胆", "龙果", "龙蒿", "龙葵", "龙荔", "龙眼", "龙珠", "蒌蒿", "蒌叶", "漏芦", "芦荟", "芦苇", "芦竹", "卤蕨"
            , "鲁桑", "鹿草", "鹿藿", "鹿药", "蕗蕨", "栾树", "乱草", "罗勒", "罗摩", "罗伞", "萝卜", "裸蒴", "裸菀", "络石", "骆骑"
            , "落葵", "落檐", "绿豆", "绿蓟", "绿萝", "葎草", "妈竹", "麻梨", "麻栎", "麻楝", "马棘", "马菅", "马兰", "马桑", "马唐"
            , "麦冬", "麦李", "蔓荆", "蔓柳", "蔓榕", "漫竹", "芒萁", "杧果", "猫乳", "毛茶", "毛茛", "毛桂", "毛菅", "毛姜", "毛蒟"
            , "毛蕨", "毛梾", "毛蓼", "毛菍", "毛茄", "毛柿", "毛桐", "毛杏", "毛榛", "毛竹", "茅根", "茅瓜", "茅栗", "茅莓", "茅香"
            , "帽柯", "玫瑰", "眉柳", "梅蓝", "美竹", "虻眼", "蒙椴", "蒙菊", "蒙桑", "米槁", "米蒿", "绵参", "绵刺", "绵竹", "棉豆"
            , "棉藜", "缅茄", "缅桐", "闽槐", "闽楠", "蘑芋", "墨兰", "墨鳞", "墨泡", "母菊", "牡丹", "牡蒿", "牡荆", "木豆", "木瓜"
            , "木荷", "木槿", "木橘", "木蓝", "木榄", "木梨", "木莲", "木蓼", "木莓", "木棉", "木薯", "木通", "木犀", "木贼", "木竹"
            , "奶桑", "南荻", "南瓜", "南蓟", "南烛", "楠草", "楠木", "楠藤", "坭藤", "坭竹", "泥柯", "拟兰", "柠檬", "牛蒡", "牛李"
            , "牛膝", "暖木", "糯竹", "女蒿", "女菀", "女萎", "女贞", "欧李", "欧芹", "爬兰", "爬苇", "泡竹", "佩兰", "喷瓜", "棚竹"
            , "蓬蘽", "枇杷", "品藻", "平竹", "苹果", "苹婆", "瓶蕨", "坡参", "坡垒", "坡柳", "珀菊", "葡蟠", "葡萄", "蒲桃", "蒲苇"
            , "朴树", "桤木", "奇蒿", "脐草", "杞柳", "牵牛", "签草", "前胡", "黔椴", "黔蕨", "芡实", "茜草", "茜树", "强竹", "墙草"
            , "乔松", "荞麦", "巧茶", "鞘花", "茄参", "窃衣", "秦艽", "秦柳", "青菜", "青冈", "青蒿", "青兰", "青裸", "青梅", "青木"
            , "青杞", "青扦", "青檀", "青葙", "青杨", "轻木", "苘麻", "筇竹", "秋枫", "秋英", "楸子", "球菊", "球兰", "球枣", "曲莲"
            , "瞿麦", "泉七", "拳参", "犬草", "雀稗", "雀苣", "雀麦", "雀瓢", "蘘荷", "荛花", "人参", "忍冬", "任豆", "绒兰", "绒藜"
            , "蓉草", "榕树", "肉桂", "肉菊", "肉兰", "汝兰", "乳豆", "乳苣", "乳茄", "乳菀", "蕤核", "蕊木", "瑞木", "瑞香", "润楠"
            , "箬竹", "赛葵", "赛楠", "三敛", "三七", "涩荠", "沙鞭", "沙参", "沙蒿", "沙棘", "沙戟", "沙芥", "沙梾", "沙梨", "沙蓬"
            , "沙穗", "沙枣", "沙针", "砂韭", "砂仁", "砂苋", "莎菀", "筛草", "山艾", "山茶", "山潺", "山橙", "山丹", "山靛", "山矾"
            , "山柑", "山蒿", "山槐", "山姜", "山橿", "山芥", "山韭", "山橘", "山蒟", "山兰", "山榄", "山楝", "山蓼", "山柳", "山椤"
            , "山莓", "山柰", "山楠", "山茄", "山芹", "山榕", "山桑", "山柿", "山桃", "山香", "山杏", "山芎", "山杨", "山枣", "山楂"
            , "山棕", "杉木", "杉松", "扇蕨", "鳝藤", "商陆", "芍药", "韶子", "蛇床", "蛇瓜", "蛇莲", "蛇莓", "蛇藤", "肾茶", "升麻"
            , "圣蕨", "石斛", "石花", "石柯", "石栗", "石莲", "石榴", "石茅", "石楠", "石松", "石韦", "石血", "石月", "石竹", "石梓"
            , "食蕨", "莳萝", "市藜", "螫麻", "匙荠", "手参", "寿竹", "绶草", "菽麻", "疏蓼", "蜀葵", "蜀枣", "鼠刺", "鼠李", "鼠茅"
            , "树参", "树棉", "双参", "水稗", "水禾", "水角", "水蕨", "水蓼", "水柳", "水龙", "水麻", "水茅", "水茄", "水芹", "水杉"
            , "水松", "水苏", "水翁", "水椰", "水芋", "水竹", "睡菜", "睡莲", "睡茄", "硕桦", "蒴莲", "丝瓜", "丝兰", "丝茅", "松蒿"
            , "菘蓝", "嵩草", "送春", "苏木", "苏铁", "肃草", "粟草", "酸橙", "酸豆", "酸模", "酸枣", "酸竹", "笋瓜", "笋兰", "梭梭"
            , "锁阳", "塔黄", "台楠", "台芋", "泰兰", "泰竹", "昙花", "檀梨", "檀香", "炭栎", "唐棣", "唐竹", "糖芥", "糖棕", "绦柳"
            , "桃榄", "藤构", "藤槐", "藤萝", "藤麻", "藤漆", "藤榕", "藤枣", "天葵", "天麻", "天蒜", "田菁", "田麻", "甜菜", "甜橙"
            , "甜麻", "甜茅", "甜杨", "甜槠", "铁榄", "铁凌", "铁椤", "铁木", "铁杉", "铁藤", "铁仔", "铁竹", "亭立", "庭荠", "庭藤"
            , "葶菊", "葶苈", "茼蒿", "桐棉", "秃茶", "秃杉", "土楠", "菟葵", "团花", "豚草", "托竹", "橐吾", "瓦松", "豌豆", "碗蕨"
            , "王瓜", "网蕨", "菵草", "苇菅", "卫矛", "猬草", "蝟菊", "蝟实", "榅桲", "文竹", "吻兰", "紊草", "紊蒿", "问荆", "蕹菜"
            , "莴苣", "倭竹", "渥丹", "乌材", "乌姜", "乌桕", "乌蕨", "乌榄", "乌柳", "乌墨", "乌柿", "乌檀", "乌药", "乌竹", "芜萍"
            , "芜青", "五加", "舞草", "西瓜", "西桦", "西柳", "菥蓂", "溪楠", "溪桫", "豨莶", "喜树", "细辛", "夏栎", "纤草", "纤柳"
            , "籼稻", "蚬木", "腺柃", "腺柳", "香橙", "香椿", "香桂", "香桦", "香槐", "香姜", "香蕉", "香兰", "香蓼", "香楠", "香茜"
            , "香芹", "香青", "香薷", "香杨", "香莸", "香橼", "香竹", "象草", "象橘", "小草", "小蜡", "小藜", "小蓬", "小茄", "缬草"
            , "薤白", "新樟", "星蕨", "杏李", "荇菜", "修蕨", "絮菊", "萱草", "玄参", "旋花", "雪胆", "雪柳", "雪松", "血桐", "血苋"
            , "荨麻", "栒子", "鸦葱", "鸭茅", "芽竹", "崖柏", "崖姜", "崖柯", "崖柳", "崖楠", "崖柿", "崖藤", "雅榕", "亚菊", "亚麻"
            , "胭木", "胭脂", "烟草", "烟豆", "烟堇", "芫花", "芫荽", "岩参", "岩风", "岩蒿", "岩桦", "岩荠", "岩蕨", "岩栎", "岩蓼"
            , "岩柃", "岩柿", "岩匙", "岩菀", "岩须", "岩芋", "岩樟", "盐蒿", "盐芥", "菴闾", "偃松", "雁茅", "燕麦", "秧青", "羊草"
            , "羊茅", "羊乳", "羊蹄", "阳荷", "阳桃", "杨梅", "杨桐", "洋椿", "洋葱", "腰果", "药蕨", "药葵", "椰子", "野桉", "野葱"
            , "野菰", "野蓟", "野蕉", "野韭", "野菊", "野葵", "野茄", "野柿", "野黍", "野桐", "野杏", "野芋", "夜花", "仪花", "益智"
            , "薏米", "薏苡", "翼蓟", "虉草", "阴香", "茵芋", "银柴", "银蒿", "银桦", "银荆", "银兰", "银柳", "银木", "银杉", "银杏"
            , "银珠", "罂粟", "樱草", "樱桃", "蘡薁", "楹树", "硬草", "硬核", "油茶", "油丹", "油桦", "油芒", "油楠", "油杉", "油柿"
            , "油松", "油桐", "油樟", "油竹", "油棕", "疣草", "莜麦", "莠竹", "柚木", "盂兰", "鱼木", "鱼藤", "俞藤", "愉柯", "榆橘"
            , "榆树", "羽茅", "雨树", "玉柏", "玉兰", "玉蕊", "玉簪", "玉竹", "郁金", "郁李", "御谷", "圆柏", "远志", "月桂", "岳桦"
            , "越桔", "越榄", "粤柳", "云杉", "云实", "云树", "芸苔", "芸香", "筠竹", "錾菜", "早竹", "蚤草", "皂荚", "皂柳", "泽漆"
            , "泽芹", "柞木", "榨菜", "粘蓼", "粘木", "獐毛", "杖藜", "杖藤", "沼菊", "沼兰", "沼柳", "沼楠", "柘树", "柘藤", "蔗茅"
            , "针茅", "知母", "栀子", "枳椇", "柊树", "柊叶", "钟兰", "帚蓼", "皱枣", "朱唇", "朱蕉", "朱槿", "朱兰", "珠蕨", "竹柏"
            , "竹芋", "竹蔗", "苎麻", "柱兰", "锥栗", "孖竹", "紫参", "紫草", "紫椿", "紫丹", "紫椴", "紫萼", "紫茎", "紫荆", "紫矿"
            , "紫柳", "紫麻", "紫楠", "紫萍", "紫萁", "紫苏"
    );
    private static final Map<Character, List<String>> MAP;
    private static final Map<String, Character> REVERSE;

    static {
        MAP = buildMapping(PLANTS.stream().distinct().toList(), SALT);
        REVERSE = MAP.entrySet().stream().flatMap(e -> e.getValue().stream().map(v -> Map.entry(v, e.getKey())))
                .collect(HashMap::new, (m, e) -> m.put(e.getKey(), e.getValue()), HashMap::putAll);
    }

    public static Map<Character, List<String>> buildMapping(List<String> plants, String salt) {
        Objects.requireNonNull(plants, "plants list required");
        if (plants.size() < BASE64_CHARS.length()) {
            throw new IllegalArgumentException("plants size must be >= 64 to cover all Base64 characters. current: " + plants.size());
        }

        // 复制并确定性打乱
        List<String> copy = new ArrayList<>(plants);
        Collections.shuffle(copy, new Random(seedFromSalt(salt)));

        Map<Character, List<String>> map = new LinkedHashMap<>(64);
        int total = copy.size();
        int base = total / BASE64_CHARS.length();
        int rem = total % BASE64_CHARS.length();

        int cursor = 0;
        for (int i = 0; i < BASE64_CHARS.length(); i++) {
            int take = base + (i < rem ? 1 : 0);
            List<String> slice = new ArrayList<>(take);
            for (int j = 0; j < take; j++) {
                slice.add(copy.get(cursor++));
            }
            map.put(BASE64_CHARS.charAt(i), Collections.unmodifiableList(slice));
        }
        return Collections.unmodifiableMap(map);
    }

    /**
     * 加密
     *
     * @param plain 明文
     * @return 加密后的字符串（由植物名拼接而成）
     */
    public static String encode(String plain) {
        return encode(plain, true);
    }

    /**
     * 加密
     *
     * @param plain    明文
     * @param compress 是否启用 gzip 压缩
     * @return 加密后的字符串（由植物名拼接而成）
     */
    public static String encode(String plain, boolean compress) {

        byte[] bytes = plain.getBytes(StandardCharsets.UTF_8);
        if (compress) {
            try {
                bytes = compress(bytes);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }

        String b64 = Base64.getEncoder().withoutPadding().encodeToString(bytes);
        StringBuilder sb = new StringBuilder(b64.length() * 2); // 预估

        ThreadLocalRandom rnd = ThreadLocalRandom.current();
        for (char c : b64.toCharArray()) {
            List<String> variants = MAP.get(c);
            if (variants == null || variants.isEmpty()) {
                throw new IllegalStateException("no variants for base64 char: " + c);
            }
            String pick = variants.get(rnd.nextInt(variants.size()));
            sb.append(pick);
        }
        return sb.toString();
    }

    /**
     * 解密
     *
     * @param cipher 密文
     * @return 解密后的明文（UTF-8）
     */
    public static String decode(String cipher) {
        return decode(cipher, true);
    }

    /**
     * 解密
     *
     * @param cipher     密文
     * @param compressed 是否启用 gzip 压缩（应与 encode 时一致）
     * @return 解密后的明文（UTF-8）
     */
    public static String decode(String cipher, boolean compressed) {
        // 为了正确切分，按 token 长度从长到短尝试匹配（防止短词优先匹配出错）
        List<String> tokensByLenDesc = new ArrayList<>(REVERSE.keySet());
        tokensByLenDesc.sort((a, b) -> Integer.compare(b.length(), a.length()));

        StringBuilder b64Builder = new StringBuilder(cipher.length() / 2);
        int i = 0;
        while (i < cipher.length()) {
            boolean matched = false;
            for (String token : tokensByLenDesc) {
                if (cipher.startsWith(token, i)) {
                    Character mapped = REVERSE.get(token);
                    if (mapped == null) {
                        throw new IllegalStateException("internal mapping missing for token: " + token);
                    }
                    b64Builder.append(mapped);
                    i += token.length();
                    matched = true;
                    break;
                }
            }
            if (!matched) {
                throw new IllegalArgumentException("cannot parse cipher at pos " + i + ": substring='" +
                        cipher.substring(i, Math.min(cipher.length(), i + 20)) + "...'. " +
                        "Possible causes: wrong plants list or salt.");
            }
        }

        String b64 = b64Builder.toString();
        // b64 是重建出来的字符串（可能无 '='）
        int mod = b64.length() % 4;
        if (mod != 0) {
            int pad = (4 - mod) % 4;
            b64 = b64 + "=".repeat(pad);
        }

        byte[] data = Base64.getDecoder().decode(b64);
        if (compressed) {
            try {
                data = decompress(data);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }
        return new String(data, StandardCharsets.UTF_8);
    }

    /**
     * 压缩字节数组
     */
    public static byte[] compress(byte[] data) throws IOException {
        if (data == null || data.length == 0) {
            return null;
        }
        try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
             GZIPOutputStream gzip = new GZIPOutputStream(bos)) {
            gzip.write(data);
            gzip.finish();
            return bos.toByteArray();
        }
    }

    /**
     * 解压字节数组
     */
    public static byte[] decompress(byte[] compressedData) throws IOException {
        if (compressedData == null || compressedData.length == 0) {
            return null;
        }
        try (ByteArrayInputStream bis = new ByteArrayInputStream(compressedData);
             GZIPInputStream gzip = new GZIPInputStream(bis);
             ByteArrayOutputStream bos = new ByteArrayOutputStream()) {
            byte[] buffer = new byte[1024];
            int len;
            while ((len = gzip.read(buffer)) > 0) {
                bos.write(buffer, 0, len);
            }
            return bos.toByteArray();
        }
    }


    /**
     * 通过 salt 生成随机种子
     */
    private static long seedFromSalt(String salt) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] digest = md.digest((salt == null ? "" : salt).getBytes(StandardCharsets.UTF_8));
            ByteBuffer bb = ByteBuffer.wrap(digest);
            return bb.getLong(); // 取前 8 字节作为 long
        } catch (Exception e) {
            return (salt == null ? 0L : (long) salt.hashCode());
        }
    }

}
