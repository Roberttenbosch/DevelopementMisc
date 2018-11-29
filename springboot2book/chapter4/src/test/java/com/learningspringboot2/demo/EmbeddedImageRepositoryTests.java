package com.learningspringboot2.demo;

import com.learningspringboot2.demo.images.domain.Image;
import com.learningspringboot2.demo.images.service.ImageRepository;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.data.mongo.DataMongoTest;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.test.context.junit4.SpringRunner;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;

import static org.assertj.core.api.Assertions.assertThat;

@RunWith(SpringRunner.class)
@DataMongoTest
public class EmbeddedImageRepositoryTests
{
    @Autowired
    ImageRepository repository;

    @Autowired
    MongoOperations operations;

    /**
     * To avoid {@code block()} calls, use blocking
     * {@link MongoOperations} during setup.
     */
    @Before
    public void setUp()
    {
        operations.dropCollection(Image.class);
        operations.insert(new Image("Id1", "name1"));
        operations.insert(new Image("Id2", "name2"));
        operations.insert(new Image("Id3", "name3"));
        operations.findAll(Image.class).forEach(image -> {
            System.out.println(image.toString());
        });
    }

    @Test
    public void findAllSuccess()
    {
        Flux<Image> images = repository.findAll();
        StepVerifier.create(images)
                .recordWith(ArrayList::new)
                .expectNextCount(3)
                .consumeRecordedWith(results ->
                {
                    assertThat(results).hasSize(3);
                    assertThat(results).extracting(Image::getName)
                            .contains("name1", "name2", "name3");
                }).expectComplete()
                .verify();
    }

    @Test
    public void findByNameSuccess()
    {
        Mono<Image> image = repository.findByName("name2");
        StepVerifier.create(image)
                .expectNextMatches(results -> {
                    assertThat(results.getName()).isEqualTo("name2");
                    assertThat(results.getId()).isEqualTo("Id2");
                    return true;
                });
    }

}
