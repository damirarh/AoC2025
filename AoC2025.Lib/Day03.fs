module AoC2025.Lib.Day03

let rec getMaximumJoltage length (bank: string) =
    if length = 0 then
        ""
    else if bank.Length = length then
        bank
    else
        let candidates = bank.Substring(0, bank.Length - length + 1).ToCharArray()
        let bestBattery = candidates |> Array.max
        let bestBatteryIndex = candidates |> Array.findIndex (fun char -> char = bestBattery)
        let remainingBank = bank.Substring(bestBatteryIndex + 1)
        $"{bestBattery}{getMaximumJoltage (length - 1) remainingBank}"

let calculateTotalJoltage count banks =
    banks
    |> List.map (getMaximumJoltage count)
    |> List.map int64
    |> List.sum
