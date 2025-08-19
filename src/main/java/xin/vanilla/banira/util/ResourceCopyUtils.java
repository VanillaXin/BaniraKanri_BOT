package xin.vanilla.banira.util;

import org.springframework.util.FileCopyUtils;

import java.io.*;
import java.net.JarURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * 资源复制工具类
 */
public class ResourceCopyUtils {

    /**
     * 递归复制 resources 下指定路径的文件到外部目录
     * 支持 jar 包和普通文件系统
     *
     * @param resourcePath resources 下相对路径
     * @param targetDir    外部目录路径
     * @return 复制的文件列表
     */
    public static List<File> copyResources(String resourcePath, String targetDir) throws IOException {
        List<File> copiedFiles = new ArrayList<>();
        URL url = ResourceCopyUtils.class.getClassLoader().getResource(resourcePath);
        if (url == null) {
            throw new FileNotFoundException("Resource path not found: " + resourcePath);
        }

        String protocol = url.getProtocol();

        if ("file".equals(protocol)) {
            // 普通文件系统
            File source = new File(url.getPath());
            copyFilesRecursively(source, new File(targetDir), copiedFiles);
        } else if ("jar".equals(protocol)) {
            // jar 内资源
            JarURLConnection jarURLConnection = (JarURLConnection) url.openConnection();
            try (JarFile jarFile = jarURLConnection.getJarFile()) {
                Enumeration<JarEntry> entries = jarFile.entries();
                String jarPathPrefix = resourcePath + "/";
                while (entries.hasMoreElements()) {
                    JarEntry entry = entries.nextElement();
                    String name = entry.getName();
                    if (name.startsWith(jarPathPrefix) && !entry.isDirectory()) {
                        String relativePath = name.substring(jarPathPrefix.length());
                        File targetFile = new File(targetDir, relativePath);
                        File parent = targetFile.getParentFile();
                        if (!parent.exists()) {
                            parent.mkdirs();
                        }
                        try (InputStream in = jarFile.getInputStream(entry);
                             FileOutputStream out = new FileOutputStream(targetFile)) {
                            FileCopyUtils.copy(in, out);
                            copiedFiles.add(targetFile);
                        }
                    }
                }
            }
        } else {
            throw new UnsupportedEncodingException("Unsupported protocol: " + protocol);
        }

        return copiedFiles;
    }

    /**
     * 递归复制文件
     */
    private static void copyFilesRecursively(File source, File target, List<File> copiedFiles) throws IOException {
        if (source.isDirectory()) {
            if (!target.exists()) {
                target.mkdirs();
            }
            File[] children = source.listFiles();
            if (children != null) {
                for (File child : children) {
                    copyFilesRecursively(child, new File(target, child.getName()), copiedFiles);
                }
            }
        } else {
            try (InputStream in = new FileInputStream(source);
                 FileOutputStream out = new FileOutputStream(target)) {
                FileCopyUtils.copy(in, out);
                copiedFiles.add(target);
            }
        }
    }
}
