package com.learningspringboot2.demo;

import com.learningspringboot2.demo.comments.controller.CommentController;
import com.learningspringboot2.demo.comments.domain.Comment;
import com.learningspringboot2.demo.images.service.ImageRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.annotation.Profile;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.concurrent.atomic.AtomicInteger;

@Profile("simulator")
@Component
public class CommentSimulator
{
    private final CommentController controller;
    private final ImageRepository repository;
    private final AtomicInteger counter;

    public CommentSimulator(CommentController controller,
                            ImageRepository repository)
    {
        this.controller = controller;
        this.repository = repository;
        this.counter = new AtomicInteger(1);
    }

    @EventListener
    public void onApplicationReadyEvent(ApplicationReadyEvent event)
    {
        Flux
                .interval(Duration.ofMillis(1000))
                .flatMap(tick -> repository.findAll())
                .map(image -> {
                    Comment comment = new Comment();
                    comment.setImageId(image.getId());
                    comment.setComment(
                            "Comment #" + counter.getAndIncrement());
                    return Mono.just(comment);
                })
                .flatMap(newComment ->
                        Mono.defer(() ->
                                controller.addComment(newComment)))
                .subscribe();
    }

}
