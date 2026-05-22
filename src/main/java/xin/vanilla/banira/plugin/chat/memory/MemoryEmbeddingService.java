package xin.vanilla.banira.plugin.chat.memory;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import xin.vanilla.banira.config.entity.extended.ChatConfig;
import xin.vanilla.banira.config.entity.extended.ChatModelEndpoint;
import xin.vanilla.banira.config.entity.extended.ChatModelSettings;
import xin.vanilla.banira.domain.AiMemory;
import xin.vanilla.banira.domain.AiMemoryEmbedding;
import xin.vanilla.banira.mapper.IAiMemoryEmbeddingDao;
import xin.vanilla.banira.mapper.param.AiMemoryEmbeddingQueryParam;
import xin.vanilla.banira.plugin.chat.ChatModelFactory;
import xin.vanilla.banira.plugin.chat.agent.AgentContext;
import xin.vanilla.banira.service.IAiMemoryManager;
import xin.vanilla.banira.util.DateUtils;
import xin.vanilla.banira.util.HttpUtils;
import xin.vanilla.banira.util.JsonUtils;
import xin.vanilla.banira.util.StringUtils;
import xin.vanilla.banira.util.http.HttpResponse;

import java.time.Duration;
import java.util.*;

/**
 * SQLite-backed semantic memory index using OpenAI-compatible embeddings.
 */
@Slf4j
@Component
public class MemoryEmbeddingService {

    private final IAiMemoryEmbeddingDao embeddingDao;

    public MemoryEmbeddingService(@Nonnull IAiMemoryEmbeddingDao embeddingDao) {
        this.embeddingDao = embeddingDao;
    }

    public void indexMemory(@Nonnull ChatConfig cfg, @Nullable AiMemory memory) {
        if (memory == null || memory.getId() == null || !cfg.memory().semanticRetrieveEnabled()) {
            return;
        }
        String content = MemorySafety.normalize(memory.getContent(), cfg);
        if (StringUtils.isNullOrEmptyEx(content)) {
            return;
        }
        EmbeddingEndpoint endpoint = resolveEndpoint(cfg);
        if (endpoint == null) {
            LOGGER.debug("skip memory embedding: no embedding endpoint configured");
            return;
        }
        try {
            List<Double> vector = embed(endpoint, content);
            if (vector.isEmpty()) {
                return;
            }
            long now = DateUtils.getTimestamp(new Date());
            embeddingDao.upsert(new AiMemoryEmbedding()
                    .setMemoryId(memory.getId())
                    .setBotId(memory.getBotId())
                    .setGroupId(memory.getGroupId())
                    .setUserId(memory.getUserId())
                    .setModelName(endpoint.modelName())
                    .setDimension(vector.size())
                    .setVectorJson(JsonUtils.toJsonString(vector))
                    .setUpdatedAt(now));
            LOGGER.debug("indexed ai memory embedding id={} dim={}", memory.getId(), vector.size());
        } catch (Exception e) {
            LOGGER.debug("memory embedding failed id={} error={}", memory.getId(), e.toString());
        }
    }

    @Nonnull
    public List<SemanticMemoryMatch> retrieve(@Nonnull AgentContext ctx
            , @Nonnull ChatConfig cfg
            , @Nonnull String query
            , @Nonnull IAiMemoryManager memoryManager
    ) {
        if (!cfg.memory().semanticRetrieveEnabled() || StringUtils.isNullOrEmptyEx(query)) {
            return List.of();
        }
        EmbeddingEndpoint endpoint = resolveEndpoint(cfg);
        if (endpoint == null) {
            return List.of();
        }
        try {
            List<Double> queryVector = embed(endpoint, query);
            if (queryVector.isEmpty()) {
                return List.of();
            }
            List<VectorRow> rows = loadRows(ctx, endpoint.modelName());
            double minScore = cfg.memory().semanticMinScore();
            int topK = Math.max(1, cfg.memory().semanticTopK());
            Map<Long, SemanticMemoryMatch> matches = new LinkedHashMap<>();
            rows.stream()
                    .map(row -> new ScoredRow(row, cosine(queryVector, row.vector())))
                    .filter(row -> row.score() >= minScore)
                    .sorted(Comparator.comparingDouble(ScoredRow::score).reversed())
                    .limit(topK)
                    .forEach(row -> {
                        AiMemory memory = memoryManager.getMemory(row.row().memoryId());
                        if (memory != null) {
                            matches.put(memory.getId(), new SemanticMemoryMatch(memory, row.score()));
                        }
                    });
            LOGGER.debug("AI semantic memory retrieve group={} user={} candidates={} matched={}",
                    ctx.scopeGroupId(), ctx.senderId(), rows.size(), matches.size());
            return new ArrayList<>(matches.values());
        } catch (Exception e) {
            LOGGER.debug("semantic memory retrieve failed group={} user={} error={}",
                    ctx.scopeGroupId(), ctx.senderId(), e.toString());
            return List.of();
        }
    }

    @Nonnull
    private List<VectorRow> loadRows(@Nonnull AgentContext ctx, @Nonnull String modelName) {
        AiMemoryEmbeddingQueryParam param = new AiMemoryEmbeddingQueryParam()
                .setBotId(ctx.botId())
                .setGroupIdInGlobal(ctx.scopeGroupId())
                .setUserIdInGlobal(ctx.senderId())
                .setModelName(modelName);
        return embeddingDao.selectByParam(param).stream()
                .map(row -> new VectorRow(row.getMemoryId(), parseVector(row.getVectorJson())))
                .filter(row -> !row.vector().isEmpty())
                .toList();
    }

    @Nonnull
    private static List<Double> embed(@Nonnull EmbeddingEndpoint endpoint, @Nonnull String input) {
        String url = ChatModelFactory.normalizeOpenAiBaseUrl(endpoint.baseUrl()) + "embeddings";
        HttpResponse response = HttpUtils.post(url)
                .header("Authorization", "Bearer " + endpoint.apiKey())
                .header("Accept", "application/json")
                .jsonBody(Map.of(
                        "model", endpoint.modelName(),
                        "input", input
                ))
                .timeout(Duration.ofSeconds(20))
                .execute();
        if (response == null || response.statusCode() < 200 || response.statusCode() >= 300) {
            LOGGER.debug("embedding endpoint failed status={}", response != null ? response.statusCode() : null);
            return List.of();
        }
        JsonObject root = JsonUtils.parseJsonObject(response.getBodyAsString());
        if (root == null || !root.has("data") || !root.get("data").isJsonArray()) {
            return List.of();
        }
        JsonArray data = root.getAsJsonArray("data");
        if (data.isEmpty() || !data.get(0).isJsonObject()) {
            return List.of();
        }
        JsonObject first = data.get(0).getAsJsonObject();
        if (!first.has("embedding") || !first.get("embedding").isJsonArray()) {
            return List.of();
        }
        return parseVector(first.getAsJsonArray("embedding"));
    }

    @Nullable
    private static EmbeddingEndpoint resolveEndpoint(@Nonnull ChatConfig cfg) {
        ChatModelSettings model = cfg.model();
        String requested = StringUtils.isNotNullOrEmpty(cfg.memory().embeddingEndpoint())
                ? cfg.memory().embeddingEndpoint().trim()
                : StringUtils.nullToEmpty(model.defaultEndpoint()).trim();
        List<ChatModelEndpoint> endpoints = model.endpoints() != null ? model.endpoints() : List.of();
        ChatModelEndpoint selected = null;
        if (StringUtils.isNotNullOrEmpty(requested)) {
            selected = endpoints.stream()
                    .filter(endpoint -> endpoint != null && endpoint.enabled())
                    .filter(endpoint -> requested.equalsIgnoreCase(endpoint.name()))
                    .findFirst()
                    .orElse(null);
        }
        if (selected == null) {
            selected = endpoints.stream()
                    .filter(endpoint -> endpoint != null && endpoint.enabled())
                    .filter(endpoint -> StringUtils.isNotNullOrEmpty(endpoint.apiKey()))
                    .findFirst()
                    .orElse(null);
        }
        if (selected == null && StringUtils.isNotNullOrEmpty(model.apiKey())) {
            selected = new ChatModelEndpoint()
                    .name("legacy")
                    .apiKey(model.apiKey())
                    .baseUrl(model.baseUrl())
                    .modelName(model.modelName())
                    .enabled(true);
        }
        if (selected == null || StringUtils.isNullOrEmptyEx(selected.apiKey())) {
            return null;
        }
        String embeddingModel = StringUtils.isNotNullOrEmpty(cfg.memory().embeddingModelName())
                ? cfg.memory().embeddingModelName().trim()
                : "text-embedding-3-small";
        String baseUrl = StringUtils.isNotNullOrEmpty(selected.baseUrl()) ? selected.baseUrl() : model.baseUrl();
        return new EmbeddingEndpoint(
                ChatModelFactory.normalizeOpenAiBaseUrl(baseUrl),
                selected.apiKey(),
                embeddingModel
        );
    }

    @Nonnull
    private static List<Double> parseVector(@Nullable String json) {
        JsonArray array = JsonUtils.parseJsonArray(json);
        return array == null ? List.of() : parseVector(array);
    }

    @Nonnull
    private static List<Double> parseVector(@Nonnull JsonArray array) {
        List<Double> vector = new ArrayList<>(array.size());
        for (JsonElement element : array) {
            if (element == null || !element.isJsonPrimitive()) {
                continue;
            }
            try {
                vector.add(element.getAsDouble());
            } catch (Exception ignored) {
            }
        }
        return vector;
    }

    private static double cosine(@Nonnull List<Double> left, @Nonnull List<Double> right) {
        if (left.isEmpty() || left.size() != right.size()) {
            return 0.0;
        }
        double dot = 0.0;
        double leftNorm = 0.0;
        double rightNorm = 0.0;
        for (int i = 0; i < left.size(); i++) {
            double l = left.get(i);
            double r = right.get(i);
            dot += l * r;
            leftNorm += l * l;
            rightNorm += r * r;
        }
        if (leftNorm <= 0 || rightNorm <= 0) {
            return 0.0;
        }
        return dot / (Math.sqrt(leftNorm) * Math.sqrt(rightNorm));
    }

    private record EmbeddingEndpoint(@Nonnull String baseUrl, @Nonnull String apiKey, @Nonnull String modelName) {
    }

    private record VectorRow(long memoryId, @Nonnull List<Double> vector) {
    }

    private record ScoredRow(@Nonnull VectorRow row, double score) {
    }

    public record SemanticMemoryMatch(@Nonnull AiMemory memory, double score) {
    }
}
