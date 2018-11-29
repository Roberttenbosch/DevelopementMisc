package com.learningspringboot2.demo.comments.service;

import com.learningspringboot2.demo.comments.domain.Comment;
import org.springframework.data.repository.Repository;
import reactor.core.publisher.Mono;

public interface CommentWriterRepository extends Repository<Comment, String>
{
    Mono<Comment> save(Comment newComment);

    Mono<Comment> findById(String id);
}
