package com.learningspringboot2.demo.comments.service;

import com.learningspringboot2.demo.comments.domain.Comment;
import io.micrometer.core.instrument.MeterRegistry;
import org.springframework.amqp.rabbit.annotation.Exchange;
import org.springframework.amqp.rabbit.annotation.Queue;
import org.springframework.amqp.rabbit.annotation.QueueBinding;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.boot.CommandLineRunner;
import org.springframework.cloud.stream.annotation.EnableBinding;
import org.springframework.cloud.stream.annotation.Input;
import org.springframework.cloud.stream.annotation.Output;
import org.springframework.cloud.stream.annotation.StreamListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Profile;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@EnableBinding(CustomProcessor.class)
public class CommentService
{

    private CommentWriterRepository repository;
    private final MeterRegistry meterRegistry;

    public CommentService(CommentWriterRepository repository, MeterRegistry meterRegistry)
    {
        this.repository = repository;
        this.meterRegistry = meterRegistry;
    }

    @RabbitListener(bindings = @QueueBinding(
            value = @Queue,
            exchange = @Exchange(value = "learning-spring-boot"),
            key = "comments.new"
    ))

    @StreamListener
    @Output(CustomProcessor.OUTPUT)
    public Flux<Void> save(@Input(CustomProcessor.INPUT)
                                   Flux<Comment> newComments) {
        return repository
                .saveAll(newComments)
                .flatMap(comment -> {
                    meterRegistry
                            .counter("comments.consumed", "imageId", comment.getImageId())
                            .increment();
                    return Mono.empty();
                });
    }

    @Bean
    @Profile("dev")
    CommandLineRunner setUpComment(MongoOperations operations)
    {
        return args -> {
            operations.dropCollection(Comment.class);
        };
    }

}
