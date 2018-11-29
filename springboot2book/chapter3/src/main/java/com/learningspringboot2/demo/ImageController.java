package com.learningspringboot2.demo;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Controller
@Slf4j
@RequestMapping("/api")
public class ImageController
{

    @GetMapping("/images")
    Flux<Image> images()
    {
        return Flux.just(
                new Image("1", "learning.jpg"),
                new Image("1", "boeren.jpg"),
                new Image("1", "blazzer.jpg")
        );
    }

    @PostMapping("/images")
    Mono<Void> create(@RequestBody Flux<Image> images)
    {
        return images
                .map(
                        image -> {
                            log.info("saving " + image + "to database");
                            return image;
                        })
                .then();
    }
}
