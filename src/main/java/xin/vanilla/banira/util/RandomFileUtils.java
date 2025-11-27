package xin.vanilla.banira.util;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Predicate;
import java.util.stream.Stream;

public final class RandomFileUtils {

    public static final String CERT_FILE_NAME = "cert.txt";

    private RandomFileUtils() {
    }

    public static Optional<File> getRandomFile(String folderPath) {
        return getRandomFile(Paths.get(folderPath), null).map(Path::toFile);
    }

    public static Optional<File> getRandomFile(String folderPath, FileFilter filter) {
        return getRandomFile(Paths.get(folderPath), filter == null ? null : p -> filter.accept(p.toFile()))
                .map(Path::toFile);
    }

    public static Optional<File> getRandomFile(File folder) {
        return getRandomFile(folder.toPath(), null).map(Path::toFile);
    }

    public static Optional<File> getRandomFile(File folder, FileFilter filter) {
        return getRandomFile(folder.toPath(), filter == null ? null : p -> filter.accept(p.toFile()))
                .map(Path::toFile);
    }

    /**
     * @return 文件的绝对路径
     */
    public static Optional<String> getRandomFileName(String folderPath) {
        return getRandomFile(Paths.get(folderPath), null).map(Path::toAbsolutePath).map(Path::toString);
    }

    /**
     * @param predicate 路径过滤器
     */
    public static Optional<Path> getRandomFile(Path folder, @Nullable Predicate<Path> predicate) {
        if (folder == null || !Files.exists(folder) || !Files.isDirectory(folder)) {
            return Optional.empty();
        }

        Predicate<Path> baseFilter = p -> Files.isRegularFile(p)
                && !CERT_FILE_NAME.equalsIgnoreCase(p.getFileName().toString());

        Predicate<Path> combined = (predicate == null) ? baseFilter : baseFilter.and(predicate);

        try (Stream<Path> stream = Files.walk(folder)) {
            Iterator<Path> it = stream.filter(combined).iterator();

            Path chosen = null;
            int count = 0;

            while (it.hasNext()) {
                Path p = it.next();
                count++;
                if (ThreadLocalRandom.current().nextInt(count) == 0) {
                    chosen = p;
                }
            }

            return Optional.ofNullable(chosen);
        } catch (IOException e) {
            return Optional.empty();
        }
    }

    /**
     * @param predicate 路径过滤器
     * @param maxDepth  最大遍历深度，0 表示无限深度
     */
    public static Optional<Path> getRandomFile(Path folder, Predicate<Path> predicate, int maxDepth) {
        if (folder == null || !Files.exists(folder) || !Files.isDirectory(folder)) {
            return Optional.empty();
        }

        Predicate<Path> baseFilter = p -> Files.isRegularFile(p)
                && !CERT_FILE_NAME.equalsIgnoreCase(p.getFileName().toString());
        Predicate<Path> combined = (predicate == null) ? baseFilter : baseFilter.and(predicate);

        try (Stream<Path> stream = (maxDepth > 0) ? Files.walk(folder, maxDepth) : Files.walk(folder)) {
            Iterator<Path> it = stream.filter(combined).iterator();

            Path chosen = null;
            int count = 0;
            while (it.hasNext()) {
                Path p = it.next();
                count++;
                if (ThreadLocalRandom.current().nextInt(count) == 0) {
                    chosen = p;
                }
            }
            return Optional.ofNullable(chosen);
        } catch (IOException e) {
            return Optional.empty();
        }
    }

    /**
     * 从指定目录中随机选取若干个文件
     *
     * @param predicate 路径过滤器
     * @param limit     最多选取的文件数
     */
    public static @Nonnull List<Path> getRandomFiles(Path folder, Predicate<Path> predicate, int limit) {
        if (limit <= 0) return List.of();

        if (folder == null || !Files.exists(folder) || !Files.isDirectory(folder)) {
            return List.of();
        }

        Predicate<Path> baseFilter = p ->
                Files.isRegularFile(p)
                        && !CERT_FILE_NAME.equalsIgnoreCase(p.getFileName().toString());

        Predicate<Path> combined = (predicate == null) ? baseFilter : baseFilter.and(predicate);

        List<Path> reservoir = new ArrayList<>(limit);
        int count = 0;

        try (Stream<Path> stream = Files.walk(folder)) {
            Iterator<Path> it = stream.filter(combined).iterator();

            while (it.hasNext()) {
                Path file = it.next();
                count++;

                if (reservoir.size() < limit) {
                    reservoir.add(file);
                } else {
                    int r = ThreadLocalRandom.current().nextInt(count);
                    if (r < limit) {
                        reservoir.set(r, file);
                    }
                }
            }
        } catch (IOException e) {
            return List.of();
        }

        return Collections.unmodifiableList(reservoir);
    }

}
