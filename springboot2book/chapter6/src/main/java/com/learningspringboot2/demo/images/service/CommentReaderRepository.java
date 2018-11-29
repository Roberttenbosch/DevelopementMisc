package com.learningspringboot2.demo.images.service;

import com.learningspringboot2.demo.images.domain.Comment;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface CommentReaderRepository extends ReactiveCrudRepository<Comment, String>
{
    Flux<Comment> findByImageId(String imageId);
}
