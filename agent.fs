open System

type AgentMessage =
    | Print of string
    | Calculate of int * int
    | GetCount of AsyncReplyChannel<int>
    | Stop

let agent =
    MailboxProcessor.Start(fun inbox ->
        let rec loop count = async {
            let! msg = inbox.Receive()
            match msg with
            | Print s ->
                printfn "Сообщение: %s" s
                return! loop (count + 1)

            | Calculate (a, b) ->
                printfn "Сумма %d и %d = %d" a b (a + b)
                return! loop (count + 1)

            | GetCount reply ->
                reply.Reply(count)
                return! loop count

            | Stop ->
                printfn "Агент остановлен"
        }
        loop 0
    )

let delayedAgent =
    MailboxProcessor.Start(fun inbox ->
        let rec loop () = async {
            let! msg = inbox.Receive()
            match msg with
            | Print s ->
                do! Async.Sleep 2000
                printfn "[Задержанное] %s" s
            | _ -> ()
            return! loop()
        }
        loop()
    )


agent.Post(Print "Hello World")
agent.Post(Calculate(10, 5))

async {
    let! count = agent.PostAndAsyncReply GetCount
    printfn "Обработано сообщений: %d" count
} |> Async.RunSynchronously

agent.Post(Stop)


delayedAgent.Post(Print "Сообщение с задержкой 2 сек")
printfn "Это сообщение появится сразу"
