---
layout: post
title:  "I found a blog, can I keep it?"
date:   2016-03-31
categories:
---
Hello, and welcome to my new blog!
For a while now, I've been switching between a few platforms
like Wordpress and Medium.
But I never liked not having a simple git repo to hold the whole blog.
Posts were managed by a magic server,
formatting was complicated,
and version control was bad or non-existent.
The solution was Jekyll.

---

Jekyll
---

This is a Jekyll blog, on GitHub Pages.
Jekyll has a lot of really nice properties.
For one, blog posts can be written in a variety of programming languages.
This post, for example, is written in markdown.
As someone who does a lot of Reddit commenting, having markdown is awesome.

Jekyll also doesn't need a backend.
It's a static site generator,
so I just write my site using layouts and pre-processed CSS,
and Jekyll makes a web page out of it.
But GitHub Pages makes things even nicer.
You only have to upload your Jekyll project,
and it'll run the generator for you.

The GitHub integration does come at a small cost though.
GitHub only supports a select few Jekyll plugins.
If you want to use a plugin, it either needs to be on their list,
or you have to build the site your self on one branch,
and publish generated HTML on `master`.
Which ultimately meant that if I wanted to use the latest version of Bootstrap,
I had to include Bootstrap sources manually.
Which turned out fine,
because that gave me access to SASS variables that I wanted to tweak.

Finally, having version control covering the entire blog is amazing.
It's really nice to feel like every aspect of my blog is in a git repo.
From layouts, to logic, to content, it's all covered.
I get to use typical git workflows, such as branching for new features.
And rolling the blog back to a previous state is a matter of a few git commands.

---

Conclusion
---

All in all, I'm very happy with this platform.
It's easy to use, I have control over every aspect, and it's very flexible.
I might miss some features of the old platforms, like analytics and such.
But I don't think there's anything on those old platforms that I value
as much as the things I get with Jekyll.

Thanks for reading,

- Will Fancher
