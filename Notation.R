setNotation<-function(useLabels) {

  a<-str_split(useLabels,";")
  
  switch(a[[1]][1],
         "psig"={pSigLabel<<-bquote(bold(p[.('sig')]))},
         "w"={pSigLabel<<-bquote(bold(w))}
         )

  LabelUD<<-a[[1]][2]

  switch(a[[1]][3],
         "+"={
           pPlus<<-TRUE
           posChar<<-"+"},
         "0"={
           pPlus<<-FALSE
           nullChar<<-"0" # '\u2013'
         },
         "-"={
           pPlus<<-FALSE
           nullChar<<-'\u2013'
         }
  )
  

if (pPlus) {
  Ptypechar<-posChar 
} else {
  Ptypechar<-nullChar
}
Ltypechar<-posChar

pPlusLabel<<-paste0("P(",Ptypechar,")")

switch (LabelUD, 
        "U"={
          Plabel<<-bquote(bold(.(Pchar)^.(Ptypechar)))
          Llabel<<-bquote(bold(.(Lchar)^.(Ltypechar)))
          
          nonNullPositive<<-bquote(.(Zchar)^.(posChar)~'+ve')  # "Z+ +ve"
          nonNullNegative<<-bquote(.(Zchar)^.(posChar) ~"-ve")  # "Z+ -ve"
          nullPositive<<-bquote(.(Zchar)^.(nullChar) ~"+ve")   # "Z0 +ve"
          nullNegative<<-bquote(.(Zchar)^.(nullChar) ~"-ve")  # "Z0 -ve"
        },
        "D"={
          Plabel<<-bquote(bold(.(Pchar)[.(Ptypechar)]))
          Llabel<<-bquote(bold(.(Lchar)[.(Ltypechar)]))
          
          nonNullPositive<<-bquote(.(Zchar)[.(posChar)] ~"+ve")  # "Z+ +ve"
          nonNullNegative<<-bquote(.(Zchar)[.(posChar)] ~"-ve")  # "Z+ -ve"
          nullPositive<<-bquote(.(Zchar)[.(nullChar)] ~"+ve")   # "Z0 +ve"
          nullNegative<<-bquote(.(Zchar)[.(nullChar)] ~"-ve")  # "Z0 -ve"
        }
)

}
