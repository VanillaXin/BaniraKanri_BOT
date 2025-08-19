package xin.vanilla.banira.util;

import java.io.*;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

public class JavaVersionUtils {

    private static final Map<Integer, String> VERSION_MAP = new HashMap<>();

    static {
        VERSION_MAP.put(45, "Java 1.1");
        VERSION_MAP.put(46, "Java 1.2");
        VERSION_MAP.put(47, "Java 1.3");
        VERSION_MAP.put(48, "Java 1.4");
        VERSION_MAP.put(49, "Java 5");
        VERSION_MAP.put(50, "Java 6");
        VERSION_MAP.put(51, "Java 7");
        VERSION_MAP.put(52, "Java 8");
        VERSION_MAP.put(53, "Java 9");
        VERSION_MAP.put(54, "Java 10");
        VERSION_MAP.put(55, "Java 11");
        VERSION_MAP.put(56, "Java 12");
        VERSION_MAP.put(57, "Java 13");
        VERSION_MAP.put(58, "Java 14");
        VERSION_MAP.put(59, "Java 15");
        VERSION_MAP.put(60, "Java 16");
        VERSION_MAP.put(61, "Java 17");
        VERSION_MAP.put(62, "Java 18");
        VERSION_MAP.put(63, "Java 19");
        VERSION_MAP.put(64, "Java 20");
        VERSION_MAP.put(65, "Java 21");
        VERSION_MAP.put(66, "Java 22");
        VERSION_MAP.put(67, "Java 23");
        VERSION_MAP.put(68, "Java 24");
        VERSION_MAP.put(69, "Java 25");
    }

    /**
     * 获取单个 Class 文件的编译版本
     *
     * @param clazz 要检查的类
     * @return Java 编译版本字符串
     */
    public static String getClassVersion(Class<?> clazz) {
        try {
            // 获取类的资源路径
            String resourcePath = clazz.getName().replace('.', '/') + ".class";
            URL resourceUrl = clazz.getClassLoader().getResource(resourcePath);

            if (resourceUrl == null) {
                return "UNKNOWN";
            }

            if ("file".equals(resourceUrl.getProtocol())) {
                return readClassVersion(new File(resourceUrl.toURI()));
            } else if ("jar".equals(resourceUrl.getProtocol())) {
                return handleJarResource(resourceUrl, resourcePath);
            }

            return "UNKNOWN";
        } catch (Exception e) {
            return "UNKNOWN";
        }
    }

    private static String handleJarResource(URL jarUrl, String resourcePath) throws IOException {
        String jarPath = jarUrl.getPath();
        int separatorIndex = jarPath.indexOf("!/");
        if (separatorIndex == -1) {
            return "UNKNOWN";
        }

        String jarFilePath = jarPath.substring(0, separatorIndex);
        if (jarFilePath.startsWith("file:")) {
            jarFilePath = jarFilePath.substring(5);
        }

        try (JarFile jarFile = new JarFile(jarFilePath)) {
            JarEntry entry = jarFile.getJarEntry(resourcePath);
            try (InputStream is = jarFile.getInputStream(entry)) {
                return readClassVersion(is);
            }
        }
    }

    private static String readClassVersion(File classFile) throws IOException {
        try (DataInputStream in = new DataInputStream(new FileInputStream(classFile))) {
            return readClassVersion(in);
        }
    }

    private static String readClassVersion(InputStream is) throws IOException {
        try (DataInputStream in = new DataInputStream(is)) {
            return readClassVersion(in);
        }
    }

    private static String readClassVersion(DataInputStream in) throws IOException {
        // 检查魔数 CAFEBABE
        if (in.readInt() != 0xCAFEBABE) return "UNKNOWN";

        // 读取 minor version (忽略)
        in.readUnsignedShort();

        // 读取 major version
        int majorVersion = in.readUnsignedShort();

        return javaVersion(majorVersion);
    }

    private static String javaVersion(int major) {
        if (VERSION_MAP.containsKey(major)) {
            return VERSION_MAP.get(major);
        } else if (major > 52) {
            return "Java " + (major - 44);
        } else {
            return "UNKNOWN (Major: " + major + ")";
        }
    }
}
