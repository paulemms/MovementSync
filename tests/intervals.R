rm(list = ls())
devtools::load_all()

df <- data.frame(id=c(rep("a",4),rep("b",2),rep("c",3)),
                 start=c(100,250,400,600,150,610,275,600,700),
                 end=c(200,300,550,650,275,640,325,675,725))


sets<-function(start, end, group, overlap=length(unique(group))) {
  dd<-rbind(data.frame(pos=start, event=1), data.frame(pos=end, event=-1))

  dd<-aggregate(event~pos, dd, sum)
  dd<-dd[order(dd$pos),]
  dd$open <- cumsum(dd$event)
  r<-rle(dd$open>=overlap)
  ex<-cumsum(r$lengths-1 + rep(1, length(r$lengths)))
  sx<-ex-r$lengths+1
  cbind(dd$pos[sx[r$values]],dd$pos[ex[r$values]+1])

}

merge_splice <- function(..., operation) {
  l <- list(...)
  stopifnot(operation %in% c('union', 'intersection'))
  overlap <- switch(operation, 'union' = 1, 'intersection' = length(l))
  segment_name <- switch(operation, 'union' = paste(names(l), collapse = "|"),
                          'intersection' = paste(names(l), collapse = "&"))

  dfr <- dplyr::bind_rows(l, .id = 'Splice')
  dd <- rbind(data.frame(pos = dfr$Start, event = 1),
              data.frame(pos = dfr$End, event = -1))

  dd <- aggregate(event ~ pos, dd, sum)
  dd <- dd[order(dd$pos), , drop=FALSE]
  dd$open <- cumsum(dd$event)
  r <- rle(dd$open >= overlap)
  ex <- cumsum(r$lengths - 1 + rep(1, length(r$lengths)))
  sx <- ex - r$lengths + 1

  output_dfr <- cbind.data.frame(
    Segment = make.unique(rep(segment_name, length(sx[r$values]))),
    Start = dd$pos[sx[r$values]],
    End = dd$pos[ex[r$values] + 1]
  )

  output_dfr
}

#union
with(df, sets(start, end, id,1))
#     [,1] [,2]
# [1,]  100  325
# [2,]  400  550
# [3,]  600  675
# [4,]  700  725

#overlap
with(df, sets(start, end, id,3))
#      [,1] [,2]
# [1,]  610  640

df <- data.frame(id=c(rep("a",4),rep("b",2),rep("c",3)),
                 start=c(100,250,400,600,150,610,275,600,700),
                 end=c(200,300,550,650,275,640,325,675,725))

l1 <- list(a1 = c(100, 200), a2 = c(250, 300), a3 = c(400, 550), a4 = c(600, 650))
split1_dfr <- splice_time(l1)
split1_dfr

l2 <- list(b1 = c(150, 275), b2 = c(610, 640))
split2_dfr <- splice_time(l2)
split2_dfr

l3 <- list(c1 = c(275, 325), c2 = c(600, 675), c3 = c(700, 725))
split3_dfr <- splice_time(l3)
split3_dfr

merge_splice(x = split1_dfr, y = split2_dfr, z = split3_dfr, operation = 'union')
merge_splice(x = split1_dfr, y = split2_dfr, z = split3_dfr, operation = 'intersection')
