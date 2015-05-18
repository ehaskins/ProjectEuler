module Problem1
    let sumDivisibleBy max divisor =
        let c = max / divisor
        divisor * (c*(c+1)) / 2

    let problem1(max) = sumDivisibleBy max 3 + sumDivisibleBy max 5 - sumDivisibleBy max 15

    let p1Solution = problem1 (1000-1)