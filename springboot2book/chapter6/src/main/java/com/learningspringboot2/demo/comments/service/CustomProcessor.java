package com.learningspringboot2.demo.comments.service;

import org.springframework.cloud.stream.annotation.Input;
import org.springframework.messaging.MessageChannel;
import org.springframework.messaging.SubscribableChannel;

public interface CustomProcessor
{
    String INPUT ="input";
    String OUTPUT ="emptyOutput";

    @Input(CustomProcessor.INPUT)
    SubscribableChannel input();

    @Input(CustomProcessor.OUTPUT)
    MessageChannel output();
}
