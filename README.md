# Tapestry

My grandma likes to sew. Years ago, she found a company who converted
photographs into stitching plans. However, then that company
disappeared; so I made her a little programme in Visual Basic (yes,
really) that did the same. It wasn't fully automatic, though: It just
"palettised" an image into a CSV file, which I then processed manually:

* Convert the numeric palette into Wingdings of increasing density.
* Take counts of each colour and matching them up to the palette. (This
  gives an estimate of how much wool is required.)
* Format the CSV nicely in Excel.
* Put everything together and print on several pieces of paper.

Note that a significant amount of preprocessing was also done to the
image to put it into a "useful" state. Ordinarily, we downsample to
eight shades of grey (as it's hard to find wool in other colours with a
good range of graduations), but this has to be done sensitively: You
need to strike a balance, in the dithering, between high-complexity
(i.e., painful to sew) and image quality. Also, it's worth pointing out
that the resolution of such embroideries is pretty low; like 100×125 at
about 12dpi.

Anyway, did I mention how long ago this was? Well, it turns out that
said VB programme doesn't work on modern platforms. Moreover, I have
less time to do all the necessary pre- and postprocessing. However, I
still want to make these things for my grandma: It gives her something
to do and the output actually looks pretty cool. So I've remade it, but
in the hope of making it fully automatic!

I was going to make it in Node.js, but I'm teaching myself Haskell and I
love it. So a Haskell implementation it is :)

## To Do List

[X] Load image
[ ] Process image: Face recognition
[ ] Process image: Downsize
[ ] Process image: Intelligent downsampling
[X] Convert image to luminance
[ ] Generate stitch pattern and palette metadata
[ ] Output to ???
