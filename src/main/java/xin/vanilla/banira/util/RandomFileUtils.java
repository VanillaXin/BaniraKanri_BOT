package xin.vanilla.banira.util;

import java.io.File;
import java.io.FileFilter;
import java.util.Optional;
import java.util.Random;

public class RandomFileUtils {
    private static final Random RANDOM = new Random();

    /**
     * 获取随机文件
     */
    public static Optional<File> getRandomFile(String folderPath) {
        return getRandomFile(new File(folderPath), null);
    }

    /**
     * 获取随机文件
     */
    public static Optional<File> getRandomFile(String folderPath, FileFilter filter) {
        return getRandomFile(new File(folderPath), filter);
    }

    /**
     * 获取随机文件
     */
    public static Optional<File> getRandomFile(File folder, FileFilter filter) {
        if (!folder.exists() || !folder.isDirectory()) {
            return Optional.empty();
        }

        File[] files = (filter == null) ?
                folder.listFiles(file -> file.isFile() && !"traversing.txt".equalsIgnoreCase(file.getName())) :
                folder.listFiles(file -> file.isFile() && !"traversing.txt".equalsIgnoreCase(file.getName()) && filter.accept(file));

        if (files == null || files.length == 0) {
            return Optional.empty();
        }

        return Optional.of(files[RANDOM.nextInt(files.length)]);
    }

    /**
     * 获取随机文件名
     */
    public static Optional<String> getRandomFileName(String folderPath) {
        Optional<File> randomFile = getRandomFile(folderPath);
        return randomFile.map(File::getAbsolutePath);
    }
}
