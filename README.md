# :warning: The `inspectr` package is now deprecated! :warning:

## Why did the package change name?
 
Due to concurrent naming issues, the `inspectr` package has been renamed as `spectacles`. 

## What do I need to do?

**Please visit [the `spectacles` project page](https://github.com/pierreroudier/spectacles), and install the `spectacles` package directly from CRAN:**

```
install.packages('spectacles')
```

## What does it means for my scripts?

The version 0.5-0 of `spectacles` is just an identical port of `inspectr` version 0.5-0, so users making the switch should only need to change the name of the library loaded in their scripts and use:

```
library(spectacles)
```