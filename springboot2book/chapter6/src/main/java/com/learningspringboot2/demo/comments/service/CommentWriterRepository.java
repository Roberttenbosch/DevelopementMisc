package com.learningspringboot2.demo.comments.service;

import com.learningspringboot2.demo.comments.domain.Comment;
import org.springframework.data.repository.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface CommentWriterRepository extends Repository<Comment, String>
{
    Mono<Comment> save(Comment newComment);
    Flux<Comment> saveAll(Flux<Comment> newComments);

    Mono<Comment> findById(String id);
}
