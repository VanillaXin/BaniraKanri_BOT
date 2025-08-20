package xin.vanilla.banira.util.html;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

@Slf4j
@Data
public class HtmlScreenshotResult {
    private final List<byte[]> bytes = new ArrayList<>();

    public boolean isEmpty() {
        return bytes.isEmpty();
    }

    public byte[] getByte() {
        return bytes.getLast();
    }

    public InputStream getInputStream() {
        return new ByteArrayInputStream(this.getByte());
    }

    public List<? extends InputStream> getInputStreams() {
        return this.bytes.stream().map(ByteArrayInputStream::new).toList();
    }

    public List<Path> writeToFiles(Path outputPath, String outputPrefix) throws IOException {
        List<Path> filePaths = new ArrayList<>();
        if (!Files.exists(outputPath)) {
            Files.createDirectories(outputPath);
        }
        IntStream.range(0, bytes.size()).parallel().forEach(i -> {
            try {
                String fileName = String.format("%s_%d.png", outputPrefix, i);
                Path filePath = outputPath.resolve(fileName);
                Files.write(filePath, bytes.get(i));
                synchronized (filePaths) {
                    filePaths.add(filePath);
                }
            } catch (IOException e) {
                LOGGER.error("Failed to write file", e);
            }
        });
        return filePaths;
    }

    public Path writeToFile(Path outputPath, String outputPrefix) throws IOException {
        if (!Files.exists(outputPath)) {
            Files.createDirectories(outputPath);
        }
        String fileName = String.format("%s.png", outputPrefix);
        Path filePath = outputPath.resolve(fileName);
        Files.write(filePath, this.getByte());
        return filePath;
    }

}
