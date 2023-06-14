package it.gov.pagopa.common.mongo.repository;

import lombok.NonNull;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.repository.query.MongoEntityInformation;
import org.springframework.data.mongodb.repository.support.SimpleMongoRepository;
import org.springframework.util.CollectionUtils;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class MongoRepositoryImpl<E, I extends Serializable> extends SimpleMongoRepository<E, I> {

    private final MongoOperations mongoOperations;
    private final MongoEntityInformation<E, I> entityInformation;

    public MongoRepositoryImpl(MongoEntityInformation<E, I> entityInformation, MongoOperations mongoOperations) {
        super(entityInformation, mongoOperations);

        this.mongoOperations = mongoOperations;
        this.entityInformation = entityInformation;
    }

    @Override
    public @NonNull Optional<E> findById(@NonNull I id) {
        List<E> result = mongoOperations.find(
                new Query(Criteria.where("_id").is(id)).cursorBatchSize(0),
                entityInformation.getJavaType(), entityInformation.getCollectionName());
        if(CollectionUtils.isEmpty(result)){
            return Optional.empty();
        } else {
            return Optional.of(result.get(0));
        }
    }

}
