library(shiny)
library(ggplot2)

set.seed(20260922)
N <- 400
# 20 bairros x 20 pessoas. População fixa e reprodutível.
pop <- data.frame(
  id = 1:N,
  bairro = rep(sprintf("B%02d", 1:20), each = 20)
)
# composição controlada: 180 homens / 220 mulheres
pop$sexo <- sample(c(rep("Homem",180), rep("Mulher",220)))
# raça/cor e idade variam por bairro para tornar conglomerados informativos
b_eff_raca <- rep(seq(-0.7,0.7,length.out=20), each=20)
b_eff_idade <- rep(seq(-0.8,0.8,length.out=20), each=20)
p_preto <- plogis(-0.35 + b_eff_raca)
pop$raca <- ifelse(runif(N) < p_preto, "Preto", "Branco")
p_idoso <- plogis(-0.75 + b_eff_idade)
pop$faixa <- ifelse(runif(N) < p_idoso, "Idoso", "Jovem")
pop$idade <- ifelse(pop$faixa=="Jovem", sample(18:59,N,replace=TRUE), sample(60:85,N,replace=TRUE))
# hipertensão: maior em idosos, homens e pretos; com variação leve por bairro
lp <- -2.65 + 1.75*(pop$faixa=="Idoso") + .38*(pop$sexo=="Homem") + .48*(pop$raca=="Preto") + rep(seq(-.22,.22,length.out=20),each=20)
pop$hipertensao <- rbinom(N,1,plogis(lp))
pop$perfil <- paste(pop$raca, pop$sexo, pop$faixa, sep=" • ")

prev_pop <- mean(pop$hipertensao)

z_conf <- function(conf) qnorm(1-(1-conf)/2)
calc_n_prop <- function(N, conf=.95, E=.05, p=.5){
  z <- z_conf(conf); n0 <- z^2*p*(1-p)/E^2
  n <- N*n0/(N-1+n0)
  ceiling(n)
}
ci_prop <- function(x, conf=.95){
  n <- length(x); ph <- mean(x); z <- z_conf(conf)
  se <- sqrt(ph*(1-ph)/n)
  c(max(0,ph-z*se), min(1,ph+z*se))
}

ui <- fluidPage(
  tags$head(tags$style(HTML("\n:root{--navy:#0b2e67;--blue:#1464c0;--soft:#f5f7fb;--border:#d9dfeb;--green:#16845b;--red:#b63b48;--muted:#667085}\nbody{background:#f6f8fc;color:#172033;font-family:system-ui,-apple-system,'Segoe UI',sans-serif}.container-fluid{max-width:1500px;padding:22px 28px}.hero{background:white;border:1px solid var(--border);border-radius:18px;padding:20px 24px;margin-bottom:16px}.hero h1{color:var(--navy);font-size:30px;margin:0 0 6px}.hero p{margin:0;color:#596273}.nav-tabs{border:0;gap:8px;margin-bottom:16px}.nav-tabs>li>a{border:1px solid var(--border)!important;border-radius:12px!important;background:white;color:var(--navy);font-weight:700}.nav-tabs>li.active>a{background:var(--navy)!important;color:white!important}.card{background:white;border:1px solid var(--border);border-radius:16px;padding:18px;margin-bottom:16px}.card h3{margin-top:0;color:var(--navy);font-size:18px}.controls{display:flex;flex-wrap:wrap;gap:10px;align-items:flex-end}.btn{border-radius:10px;font-weight:650}.btn-primary{background:var(--navy);border-color:var(--navy)}.btn-success{background:var(--green);border-color:var(--green)}.people-wrap{overflow:auto;padding:6px 5px 10px}.people{display:grid;grid-template-columns:repeat(20,32px);gap:5px;min-width:735px}.person{width:32px;height:39px;border:1px solid #cbd3e1;border-radius:9px 9px 7px 7px;background:#f1f3f7;position:relative;cursor:pointer;padding:0}.person:before{content:'';position:absolute;width:12px;height:12px;border-radius:50%;top:4px;left:9px;background:var(--skin,#ddd);border:1px solid rgba(0,0,0,.15)}.person:after{content:'';position:absolute;width:16px;height:15px;bottom:4px;left:7px;border-radius:5px 5px 3px 3px;background:var(--cloth,#8aa)}.person.sel{box-shadow:0 0 0 3px #f2b134;background:#fff7df;z-index:2}.person.sample{box-shadow:0 0 0 3px var(--blue);z-index:2}.person.sel.sample{box-shadow:0 0 0 3px var(--blue),0 0 0 6px #f2b134}.person.elder:after{height:13px}.bairro-label{font-size:11px;color:#778196;text-align:center;margin-top:2px}.legend{display:flex;gap:14px;flex-wrap:wrap;font-size:12px;color:#5c6678;margin:8px 0}.dot{display:inline-block;width:10px;height:10px;border-radius:50%;margin-right:4px}.metric-grid{display:grid;grid-template-columns:repeat(4,minmax(130px,1fr));gap:10px}.metric-grid.five{grid-template-columns:repeat(5,minmax(115px,1fr))}.metric{background:#f8faff;border:1px solid #e1e6ef;border-radius:12px;padding:13px}.metric .k{font-size:12px;color:#687386}.metric .v{font-size:24px;font-weight:800;color:var(--navy)}.note{background:#f3f7ff;border-left:4px solid var(--blue);padding:12px 14px;border-radius:8px;margin-top:10px}.warn{background:#fff6e7;border-left-color:#d58a00}.formula{font-family:Georgia,serif;font-size:18px}.sim-scroll{max-height:560px;overflow:auto}.smallmuted{font-size:12px;color:var(--muted)}.method-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:10px}.method-grid .btn{white-space:normal;height:100%;min-height:52px}.sample-counter{display:flex;align-items:center;justify-content:space-between;gap:10px;background:#f3f7ff;border:1px solid #dce7fb;border-radius:12px;padding:10px 12px;margin-bottom:10px}.sample-counter .n{font-size:24px;font-weight:800;color:var(--navy)}.p1-results{display:grid;grid-template-columns:1.2fr .8fr;gap:16px}.single-ic-wrap{margin-top:14px}.author-box{position:relative;overflow:hidden;display:flex;align-items:center;gap:20px;border:1.5px solid var(--navy);border-radius:16px;padding:16px 22px;background:#fff;margin-top:18px}.author-box:after{content:'';position:absolute;right:-15px;bottom:-28px;width:260px;height:120px;opacity:.07;background-image:radial-gradient(circle,var(--navy) 2.5px,transparent 2.7px);background-size:16px 16px;transform:rotate(-8deg);pointer-events:none}.logo-wrap{width:165px;flex:0 0 165px;text-align:center;position:relative;z-index:1}.logo-wrap img{max-width:150px;max-height:105px;object-fit:contain}.author-divider{width:1px;align-self:stretch;background:#aeb8c7;position:relative;z-index:1}.author-text{position:relative;z-index:1}.author-name{color:var(--navy);font-size:17px;font-weight:800;margin-bottom:6px}.author-line{font-size:15px;margin:3px 0;color:#4d596b}@media(max-width:800px){.author-box{align-items:flex-start;gap:12px;padding:14px}.logo-wrap{width:105px;flex-basis:105px}.logo-wrap img{max-width:100px;max-height:80px}.author-line{font-size:13px}}@media(max-width:460px){.author-box{flex-direction:column;align-items:center;text-align:center}.author-divider{width:100%;height:1px}.logo-wrap{width:100%;flex-basis:auto}}@media(max-width:1100px){.metric-grid.five{grid-template-columns:repeat(3,1fr)}}@media(max-width:800px){.container-fluid{padding:12px}.metric-grid{grid-template-columns:repeat(2,1fr)}.hero h1{font-size:25px}.method-grid{grid-template-columns:1fr}.p1-results{grid-template-columns:1fr}.card{padding:14px}.people{grid-template-columns:repeat(10,32px);min-width:365px}.people-wrap{overflow-x:auto}.nav-tabs>li>a{font-size:12px;padding:9px 8px}}\n"))),
  div(class="hero", h1("Laboratório de Amostragem e Inferência"), p("Explore como a forma de selecionar, o tamanho da amostra e a incerteza influenciam as conclusões sobre uma população.")),
  tabsetPanel(id="painel",
    tabPanel("1  Como selecionar?",
      div(class="card",
          h3("Como vamos selecionar a amostra?"),
          div(class="method-grid",
              actionButton("modo_manual","Monte você mesmo",class="btn-default"),
              actionButton("aas40","Sorteie 40 entre as 400",class="btn-primary"),
              actionButton("sist40","Sorteie uma das 10 primeiras e selecione 1 a cada 10",class="btn-primary"),
              actionButton("estr40","Sorteie 18 homens e 22 mulheres",class="btn-primary"),
              actionButton("cong40","Sorteie 2 bairros e selecione todos os moradores",class="btn-primary"),
              actionButton("limpar1","Limpar amostra")
          )
      ),
      div(class="p1-results",
          div(class="card", h3("Resultado da amostra"), plotOutput("bar_amostra",height=220), uiOutput("resultado_p1")),
          div(class="card", h3("Comparação com a população"), actionButton("comparar1","Comparar com a população",class="btn-success"), uiOutput("comparacao1"))
      ),
      div(class="card", h3("População fictícia: 400 moradores em 20 bairros"),
          p("Cada símbolo representa uma pessoa. Sexo, raça/cor e faixa etária são visíveis; hipertensão permanece oculta durante a seleção."),
          uiOutput("contador_p1"),
          div(class="legend", span(HTML("● <b>Cor do corpo</b>: raça/cor")), span(HTML("■ <b>Cor da roupa</b>: sexo")), span("Contorno azul = amostra selecionada"), span("Contorno amarelo = marcação manual")),
          div(class="people-wrap", uiOutput("pessoas_ui")),
          conditionalPanel("output.modoManualAtivo", div(style="margin-top:12px", actionButton("confirmar_manual","Selecionar os marcados",class="btn-success"))),
          p(class="smallmuted","Os bairros são os blocos B01 a B20, com 20 moradores cada."))
    ),
    tabPanel("2  Quantas pessoas?",
      fluidRow(
        column(7,
          div(class="card", h3("Variabilidade e tamanho da amostra"),
              sliderInput("n2","Tamanho da amostra aleatória simples",min=10,max=300,value=40,step=10),
              actionButton("sortear2","Sortear nova amostra",class="btn-primary"),
              actionButton("repetir20","Sortear 20 amostras",class="btn-default"),
              actionButton("limpar2","Limpar gráfico",class="btn-default"),
              plotOutput("graf_n",height=300), uiOutput("res2"))
        ),
        column(5,
          div(class="card", h3("Planejar o tamanho da amostra"),
              numericInput("Ncalc","Tamanho da população (N)",400,min=20,step=10),
              selectInput("confcalc","Nível de confiança",choices=c("90%"=.90,"95%"=.95,"99%"=.99),selected=.95),
              sliderInput("Ecalc","Erro amostral tolerável",min=.02,max=.15,value=.05,step=.01,post=""),
              sliderInput("pcalc","Proporção esperada",min=.05,max=.95,value=.50,step=.05),
              uiOutput("calc_n"),
              div(class="note",HTML("Para <b>p = 50%</b>, a variabilidade p(1−p) é máxima; por isso, é uma escolha conservadora quando não há estimativa prévia.")))
        )
      )
    ),
    tabPanel("3  Da amostra para a população",
      fluidRow(
        column(5,
          div(class="card", h3("Inferência a partir de uma amostra"),
              radioButtons("fonte3","Usar amostra do:",choices=c("Painel 1"="p1","Painel 2"="p2"),inline=TRUE),
              selectInput("conf3","Nível de confiança",choices=c("90%"=.90,"95%"=.95,"99%"=.99),selected=.95),
              actionButton("calc3","Calcular intervalo e estimativa",class="btn-primary"),
              uiOutput("infer3"),
              div(class="single-ic-wrap", plotOutput("graf_ic_single",height=230)),
              actionButton("revelar3","Revelar parâmetro populacional",class="btn-success"))
        ),
        column(7,
          div(class="card", h3("Repetição do processo"),
              div(class="controls",
                  numericInput("n100","Tamanho de cada amostra",40,min=10,max=300,step=10),
                  actionButton("sim100","Gerar 100 amostras aleatórias",class="btn-primary")),
              uiOutput("sum100")),
          div(class="card sim-scroll", plotOutput("graf_ic",height=1200))
        )
      )
    )
  ),
  div(
    class="author-box",
    div(class="logo-wrap", tags$img(src="lameq.svg", alt="Logo do LAMEQ")),
    div(class="author-divider"),
    div(
      class="author-text",
      div("Davi da Silveira Barroso Alves", class="author-name"),
      div("Laboratório de Métodos Quantitativos Aplicados (LAMEQ)", class="author-line"),
      div("Universidade Federal do Estado do Rio de Janeiro (UNIRIO)", class="author-line")
    )
  )
)

server <- function(input, output, session){
  rv <- reactiveValues(sel=integer(0), sample1=integer(0), method1=NULL, manual=FALSE, reveal1=FALSE,
                       sample2=integer(0), hist2=data.frame(), reveal3=FALSE, single3=NULL, sim=NULL)


  output$modoManualAtivo <- reactive({ isTRUE(rv$manual) })
  outputOptions(output, "modoManualAtivo", suspendWhenHidden = FALSE)
  output$contador_p1 <- renderUI({
    nmark <- if(isTRUE(rv$manual)) length(unique(rv$sel)) else length(rv$sample1)
    lab <- if(isTRUE(rv$manual)) "Pessoas marcadas" else "Tamanho da amostra"
    div(class="sample-counter", span(lab), span(class="n", nmark))
  })
  output$pessoas_ui <- renderUI({
    # 20 blocos de bairro, cada um com 20 pessoas
    tags$div(class="people",
      lapply(1:N,function(i){
        p <- pop[i,]
        cls <- paste("person", if(p$faixa=="Idoso") "elder" else "", if(i %in% rv$sel) "sel" else "", if(i %in% rv$sample1) "sample" else "")
        skin <- if(p$raca=="Preto") "#5b4032" else "#e8c5a5"
        cloth <- if(p$sexo=="Homem") "#4e78a8" else "#a85f87"
        actionButton(paste0("p_",i),label=NULL,class=cls,style=paste0("--skin:",skin,";--cloth:",cloth,";"),title=paste(p$id,p$bairro,p$perfil))
      })
    )
  })
  # Observers for person buttons
  lapply(1:N,function(i){ observeEvent(input[[paste0("p_",i)]],{
    if(isTRUE(rv$manual)){ if(i %in% rv$sel) rv$sel <- setdiff(rv$sel,i) else rv$sel <- c(rv$sel,i) }
  },ignoreInit=TRUE) })

  observeEvent(input$modo_manual,{rv$sel<-integer(0);rv$sample1<-integer(0);rv$method1<-"Seleção manual";rv$manual<-TRUE;rv$reveal1<-FALSE})
  observeEvent(input$confirmar_manual,{rv$sample1<-sort(unique(rv$sel));rv$method1<-"Seleção manual";rv$reveal1<-FALSE})
  observeEvent(input$aas40,{rv$manual<-FALSE;rv$sample1<-sort(sample(pop$id,40));rv$sel<-integer(0);rv$method1<-"Sorteio de 40 pessoas";rv$reveal1<-FALSE})
  observeEvent(input$sist40,{
    rv$manual<-FALSE; ini<-sample(1:10,1); rv$sample1<-seq(ini,N,by=10);rv$sel<-integer(0);rv$method1<-paste0("Início ",ini,"; depois 1 a cada 10");rv$reveal1<-FALSE
  })
  observeEvent(input$estr40,{
    rv$manual<-FALSE; h<-sample(pop$id[pop$sexo=="Homem"],18);m<-sample(pop$id[pop$sexo=="Mulher"],22)
    rv$sample1<-sort(c(h,m));rv$sel<-integer(0);rv$method1<-"18 homens e 22 mulheres";rv$reveal1<-FALSE
  })
  observeEvent(input$cong40,{
    rv$manual<-FALSE; b<-sample(unique(pop$bairro),2);rv$sample1<-pop$id[pop$bairro%in%b];rv$sel<-integer(0);rv$method1<-paste("Bairros",paste(b,collapse=" e "));rv$reveal1<-FALSE
  })
  observeEvent(input$limpar1,{rv$sel<-integer(0);rv$sample1<-integer(0);rv$method1<-NULL;rv$manual<-FALSE;rv$reveal1<-FALSE})
  observeEvent(input$comparar1,{if(length(rv$sample1)>0) rv$reveal1<-TRUE})

  output$bar_amostra <- renderPlot({
    req(length(rv$sample1)>0); s<-pop[rv$sample1,]; dd<-data.frame(Status=c("Sem hipertensão","Hipertensão"),Prop=c(mean(s$hipertensao==0),mean(s$hipertensao==1)))
    ggplot(dd,aes(Status,Prop,fill=Status))+geom_col(width=.62,show.legend=FALSE)+geom_text(aes(label=scales::percent(Prop,accuracy=1)),vjust=-.5,fontface="bold",size=5)+scale_y_continuous(labels=scales::percent,limits=c(0,1))+labs(x=NULL,y="Proporção na amostra")+theme_minimal(base_size=13)+theme(panel.grid.minor=element_blank())
  })
  output$resultado_p1 <- renderUI({
    if(!length(rv$sample1)) return(div(class="note","Selecione uma amostra para revelar sua composição."))
    s<-pop[rv$sample1,]; div(class="metric-grid",
      div(class="metric",div(class="k","Tamanho"),div(class="v",length(rv$sample1))),
      div(class="metric",div(class="k","Hipertensão"),div(class="v",scales::percent(mean(s$hipertensao),accuracy=.1))),
      div(class="metric",div(class="k","Homens"),div(class="v",scales::percent(mean(s$sexo=="Homem"),accuracy=1))),
      div(class="metric",div(class="k","Idosos"),div(class="v",scales::percent(mean(s$faixa=="Idoso"),accuracy=1))))
  })
  output$comparacao1 <- renderUI({
    if(!rv$reveal1) return(div(class="note","A proporção verdadeira permanece oculta até você comparar."))
    s<-pop[rv$sample1,]; ph<-mean(s$hipertensao); err<-ph-prev_pop
    tagList(hr(),p(strong("População: "),scales::percent(prev_pop,accuracy=.1)," com hipertensão"),p(strong("Amostra: "),scales::percent(ph,accuracy=.1)),p(strong("Erro amostral observado: "),sprintf("%+.1f pontos percentuais",100*err)))
  })

  observeEvent(input$sortear2,{
    n<-min(input$n2,N); rv$sample2<-sort(sample(pop$id,n)); ph<-mean(pop$hipertensao[rv$sample2]); rv$hist2<-rbind(rv$hist2,data.frame(ord=nrow(rv$hist2)+1,n=n,prop=ph))
  })
  observeEvent(input$repetir20,{
    n<-min(input$n2,N); vals<-replicate(20,mean(pop$hipertensao[sample(pop$id,n)])); rv$hist2<-rbind(rv$hist2,data.frame(ord=seq(nrow(rv$hist2)+1,length.out=20),n=n,prop=vals)); rv$sample2<-sort(sample(pop$id,n))
  })
  observeEvent(input$limpar2,{rv$sample2<-integer(0);rv$hist2<-data.frame()})
  output$graf_n <- renderPlot({
    if(!nrow(rv$hist2)) return(NULL)
    ggplot(rv$hist2,aes(ord,prop))+geom_hline(yintercept=prev_pop,linetype=2)+geom_point(aes(size=n),alpha=.8)+scale_y_continuous(labels=scales::percent,limits=c(0,max(.7,max(rv$hist2$prop)+.05)))+labs(x="Sorteio",y="Proporção de hipertensão",size="n")+theme_minimal(base_size=13)
  })
  output$res2 <- renderUI({
    if(!length(rv$sample2)) return(div(class="note","Escolha n e sorteie uma amostra."))
    ph <- mean(pop$hipertensao[rv$sample2])
    casos <- sum(pop$hipertensao[rv$sample2])
    erro_medio_abs <- if(nrow(rv$hist2)) mean(abs(rv$hist2$prop - prev_pop)) else abs(ph-prev_pop)
    tagList(
      div(class="metric-grid five",
          div(class="metric",div(class="k","n"),div(class="v",length(rv$sample2))),
          div(class="metric",div(class="k","Hipertensos na amostra"),div(class="v",casos)),
          div(class="metric",div(class="k","Estimativa"),div(class="v",scales::percent(ph,accuracy=.1))),
          div(class="metric",div(class="k","Erro observado"),div(class="v",sprintf("%+.1f pp",100*(ph-prev_pop)))),
          div(class="metric",div(class="k","Erro médio absoluto"),div(class="v",sprintf("%.1f pp",100*erro_medio_abs)))
      ),
      p(class="smallmuted","O erro médio absoluto resume, sem cancelamento entre erros positivos e negativos, a distância média das estimativas acumuladas até a proporção populacional. Use Limpar gráfico antes de comparar uma nova sequência de tamanhos amostrais.")
    )
  })
  output$calc_n <- renderUI({
    conf<-as.numeric(input$confcalc); E<-input$Ecalc;p<-input$pcalc;NN<-input$Ncalc;z<-z_conf(conf);n0<-z^2*p*(1-p)/E^2;n<-calc_n_prop(NN,conf,E,p)
    tagList(div(class="metric-grid",div(class="metric",div(class="k","População muito grande"),div(class="v",ceiling(n0))),div(class="metric",div(class="k","Com correção para N"),div(class="v",n))),p(class="smallmuted",sprintf("z = %.3f; erro = %.0f%%; p esperada = %.0f%%",z,E*100,p*100)))
  })

  observeEvent(input$calc3,{
    rv$reveal3<-FALSE
    ids <- if(input$fonte3=="p1") rv$sample1 else rv$sample2
    if(length(ids)){
      x <- pop$hipertensao[ids]; conf <- as.numeric(input$conf3); ci <- ci_prop(x,conf)
      rv$single3 <- data.frame(amostra="Amostra selecionada", est=mean(x), lo=ci[1], hi=ci[2])
    } else rv$single3 <- NULL
  })
  observeEvent(input$revelar3,{rv$reveal3<-TRUE})
  fonte_ids <- reactive({if(input$fonte3=="p1") rv$sample1 else rv$sample2})
  output$infer3 <- renderUI({
    input$calc3; ids<-fonte_ids(); if(!length(ids)) return(div(class="note warn","Ainda não há uma amostra disponível nesse painel."))
    x<-pop$hipertensao[ids]; conf<-as.numeric(input$conf3);ci<-ci_prop(x,conf);ph<-mean(x); estN<-round(ph*N); w<-N/length(ids)
    extra<-if(input$fonte3=="p1" && identical(rv$method1,"Seleção manual")) div(class="note warn",HTML("<b>Atenção:</b> na seleção manual, probabilidades de inclusão não são conhecidas. A expansão N/n abaixo é apenas ilustrativa e não justifica inferência probabilística.")) else NULL
    tagList(div(class="metric-grid",div(class="metric",div(class="k","Prevalência estimada"),div(class="v",scales::percent(ph,accuracy=.1))),div(class="metric",div(class="k",paste0("IC",round(conf*100),"%")),div(class="v",paste0(scales::percent(ci[1],accuracy=.1),"–",scales::percent(ci[2],accuracy=.1)))),div(class="metric",div(class="k","Hipertensos estimados em 400"),div(class="v",estN)),div(class="metric",div(class="k","Peso simples N/n"),div(class="v",sprintf("%.1f",w)))),
      div(class="note",HTML(sprintf("Na expansão simples, cada pessoa da amostra representa aproximadamente <b>%.1f</b> pessoas da população.",w))),extra,
      if(rv$reveal3) div(class="note",HTML(sprintf("Parâmetro populacional revelado: <b>%s</b> (%d hipertensos em 400).",scales::percent(prev_pop,accuracy=.1),sum(pop$hipertensao)))) else NULL)
  })

  output$graf_ic_single <- renderPlot({
    req(rv$single3); d <- rv$single3
    g <- ggplot(d,aes(y=amostra,x=est))+
      geom_segment(aes(x=lo,xend=hi,yend=amostra),linewidth=2.2,color="#1464c0")+
      geom_point(size=4,color="#0b2e67")+
      scale_x_continuous(labels=scales::percent,limits=c(0,1))+
      labs(x=paste0("Estimativa e IC",round(as.numeric(input$conf3)*100),"%"),y=NULL)+
      theme_minimal(base_size=13)+theme(panel.grid.minor=element_blank(),axis.text.y=element_text(face="bold"))
    if(isTRUE(rv$reveal3)) g <- g + geom_vline(xintercept=prev_pop,linetype=2,linewidth=1.1,color="#b63b48") + annotate("text",x=prev_pop,y=1.28,label=paste0("Parâmetro = ",scales::percent(prev_pop,accuracy=.1)),hjust=-.05,color="#b63b48",fontface="bold",size=4)
    g
  })

  observeEvent(input$sim100,{
    n<-min(max(10,input$n100),N-1); conf<-as.numeric(input$conf3)
    sims<-lapply(1:100,function(j){ids<-sample(pop$id,n);x<-pop$hipertensao[ids];ci<-ci_prop(x,conf);data.frame(amostra=j,est=mean(x),lo=ci[1],hi=ci[2],cobre=ci[1]<=prev_pop & ci[2]>=prev_pop)})
    rv$sim<-do.call(rbind,sims)
  })
  output$graf_ic <- renderPlot({
    req(rv$sim);d<-rv$sim
    ggplot(d,aes(y=amostra,x=est,color=cobre))+geom_vline(xintercept=prev_pop,linetype=2)+geom_segment(aes(x=lo,xend=hi,yend=amostra),linewidth=.7)+geom_point(size=1.7)+scale_x_continuous(labels=scales::percent)+scale_color_manual(values=c("TRUE"="#287a55","FALSE"="#c34755"),labels=c("TRUE"="Contém o parâmetro","FALSE"="Não contém"))+labs(x="Proporção / intervalo de confiança",y="Amostra",color=NULL)+theme_minimal(base_size=12)+theme(legend.position="top",panel.grid.minor=element_blank())
  })
  output$sum100 <- renderUI({req(rv$sim);k<-sum(rv$sim$cobre);div(class="note",HTML(sprintf("<b>%d de 100</b> intervalos incluíram a verdadeira proporção populacional.",k)))})
}

shinyApp(ui,server)
