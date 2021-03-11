Good job on this!

1. Nice! You are using the branch-merge pipeline. What do you think of it?

2. Make sure that you remember, though, that the master branch is the only one anybody will ever look at. You should think of the other branches as "rough drafts" and the master branch as the current draft. So merge all of your work before the final submission.

3. Don't forget to update your README for the final submission. Think of it as an executive summary of your project for people who have no idea about it. So it should describe the main points and where folks should look if they want to learn more. Here is an example from my own work: <https://github.com/dcgerard/updog>

4. I like the idea of studying on-time performance with region. Another idea is on-time performance vs airport size --- just from personal experience it seems that larger airports are more delayed (think LAX, JFK, ORD). If that's the case, then you would probably have to adjust for airport size when looking at performance by region --- since some regions have smaller airports on average.

5. I like the idea of looking at performance vs traffic --- you're kind of asking which airports perform best at scale.

6. For the shiny app, are you thinking about doing this via predictive modeling (e.g. random forrest or logistic regression) or via raw proportions?

7. If the data are too large, don't be afraid to subselect airports/years/variables and narrow down your questions.

8. Don't be afraid to break up Rmd files. You never want to have too much in one file --- it becomes too hard to read and get the point. Ideally, Rmd files do one or two simple things and come to a small conclusion. A complex data science project builds off of these small conclusions.
