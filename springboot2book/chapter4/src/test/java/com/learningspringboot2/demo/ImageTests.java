package com.learningspringboot2.demo;

import com.learningspringboot2.demo.images.domain.Image;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class ImageTests
{
    @Test
    public void imagesManagedByLombokShouldWork()
    {
        Image image = new Image("id", "file-name.jpg");
        assertThat(image.getId()).isEqualTo("id");
        assertThat(image.getName()).isEqualTo("file-name.jpg");
    }
}
