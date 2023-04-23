from scipy.stats import norm
import math
import pytest
import matplotlib.pyplot as plt
import numpy as np

def calculateD_1(initPrice, K, drift, time, volatility):
    d_1 = (np.log(initPrice/K) + (drift + (volatility**2)/2) * time)/(volatility * math.sqrt(time))
    return d_1

def calculateBScall(initPrice, K, drift, time, volatility):
    d_1 = calculateD_1(initPrice, K, drift, time, volatility)
    BS_call = (initPrice * norm.cdf(d_1)) - (K * (math.exp((-1)*drift*time)) * norm.cdf(d_1 - (volatility * math.sqrt(time))))
    return BS_call

def calculateBSput(initPrice, K, drift, time, volatility):
    d_1 = calculateD_1(initPrice, K, drift, time, volatility)
    BS_call = (K * (math.exp((-1)*drift*time)) * norm.cdf((volatility * math.sqrt(time)) - d_1)) - (initPrice * norm.cdf((-1) * d_1))
    return BS_call

def calculateDeltaCall(initPrice, K, drift, time, volatility):
    d_1 = calculateD_1(initPrice, K, drift, time, volatility)
    return (norm.cdf(d_1))

def calculateDeltaPut(initPrice, K, drift, time, volatility):
    d_1 = calculateD_1(initPrice, K, drift, time, volatility)
    return ((-1) * norm.cdf((-1) * d_1))

def callPutParityDifferences(initPrice, K, drift, time, volatility):
    deltaCall = calculateDeltaCall(initPrice, K, drift, time, volatility)
    deltaPut = calculateDeltaPut(initPrice, K, drift, time, volatility)
    valueCall = calculateBScall(initPrice, K, drift, time, volatility)
    valuePut = calculateBSput(initPrice, K, drift, time, volatility)
    return ((deltaCall-deltaPut), (valueCall-valuePut))

@pytest.mark.parametrize(
    "initPrice, K, drift, time, volatility, expectedResult",
    [
        (100, 120, 0.05, 1, 0.2, (1.0, round(100-(120*math.exp(-0.05*1)), 10))),
        (100, 50, 0.05, 1, 0.2, (1.0, round(100-(50*math.exp(-0.05*1)), 10))),
        (100, 120, 0.35, 1, 0.2, (1.0, round(100-(120*math.exp(-0.35*1)), 10))),
        (100, 120, 0.15, 3.5, 0.2, (1.0, round(100-(120*math.exp(-0.15*3.5)), 10))),
        (100, 120, 0.35, 0.4, 0.6, (1.0, round(100-(120*math.exp(-0.35*0.4)), 10))),
        (100, 150, 0.2, 2.333, 0.2, (1.0, round(100-(150*math.exp(-0.2*2.333)), 10))),
        (50, 40, 0.1, 5, 0.1, (1.0, round(50-(40*math.exp(-0.1*5)), 10))),
        (50, 30, 0.5, 3, 0.1, (1.0, round(50-(30*math.exp(-0.5*3)), 10))),
        (50, 10, 0.999, 0.5, 0.1, (1.0, round(50-(10*math.exp(-0.999*0.5)), 10))),
        (50, 500, 0.7, 2, 0.1, (1.0, round(50-(500*math.exp(-0.7*2)), 10)))
    ]
)
def testCallPutParity(initPrice, K, drift, time, volatility, expectedResult):
    resultTuple = callPutParityDifferences(initPrice, K, drift, time, volatility)
    assert expectedResult == (resultTuple[0], round(resultTuple[1], 10))

def plotCallValues():

    x = np.linspace(30,300, num=2700)
    y_001 = calculateBScall(x,120,0.05,0.001,0.1)
    y_05 = calculateBScall(x,120,0.05,0.5,0.1)
    y_1 = calculateBScall(x,120,0.05,1,0.1)

    # plt.plot(x, calculateBScall(x,120,0.05,0.001,0.1))
    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(30,300)
    ax.set_ylim(-5,200)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Option value')
    ax.set_title('Call option values for different time to maturity')
    ax.legend()

    plt.savefig('call_option.png')

    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(80,150)
    ax.set_ylim(-5,75)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Option value')
    ax.set_title('Call option values for different time to maturity')
    ax.legend()

    plt.savefig('call_option_zoom.png')

    plt.show()

def plotPutValues():

    x = np.linspace(30,300, num=2700)
    y_001 = calculateBSput(x,120,0.05,0.001,0.1)
    y_05 = calculateBSput(x,120,0.05,0.5,0.1)
    y_1 = calculateBSput(x,120,0.05,1,0.1)

    # plt.plot(x, calculateBScall(x,120,0.05,0.001,0.1))
    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(30,200)
    ax.set_ylim(-5,200)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Option value')
    ax.set_title('Put option values for different time to maturity')
    ax.legend(loc='upper left')

    plt.savefig('put_option.png')

    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(80,150)
    ax.set_ylim(-5,75)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Option value')
    ax.set_title('Put option values for different time to maturity')
    ax.legend()

    plt.savefig('put_option_zoom.png')

    plt.show()

def plotCallDeltas():

    x = np.linspace(30,300, num=2700)
    y_001 = calculateDeltaCall(x,120,0.05,0.001,0.1)
    y_05 = calculateDeltaCall(x,120,0.05,0.5,0.1)
    y_1 = calculateDeltaCall(x,120,0.05,1,0.1)

    # plt.plot(x, calculateBScall(x,120,0.05,0.001,0.1))
    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(30,300)
    ax.set_ylim(-0.2,2)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Delta of a call option')
    ax.set_title('Deltas of a call option for different time to maturity')
    ax.legend()

    plt.savefig('call_delta.png')

    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(80,150)
    ax.set_ylim(-0.2,2)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Delta of a call option')
    ax.set_title('Deltas of a call option for different time to maturity')
    ax.legend()

    plt.savefig('call_delta_zoom.png')

    plt.show()

def plotPutDeltas():

    x = np.linspace(30,300, num=2700)
    y_001 = calculateDeltaPut(x,120,0.05,0.001,0.1)
    y_05 = calculateDeltaPut(x,120,0.05,0.5,0.1)
    y_1 = calculateDeltaPut(x,120,0.05,1,0.1)

    # plt.plot(x, calculateBScall(x,120,0.05,0.001,0.1))
    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(30,300)
    ax.set_ylim(-1.2,0.5)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Delta of a put option')
    ax.set_title('Deltas of a put option for different time to maturity')
    ax.legend()

    plt.savefig('put_delta.png')

    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(80,150)
    ax.set_ylim(-1.2,0.5)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Delta of a put option')
    ax.set_title('Deltas of a put option for different time to maturity')
    ax.legend()

    plt.savefig('put_delta_zoom.png')

    plt.show()

def plotCallValues20():

    x = np.linspace(30,300, num=2700)
    y_001 = calculateBScall(x,120,0.05,0.001,0.2)
    y_05 = calculateBScall(x,120,0.05,0.5,0.2)
    y_1 = calculateBScall(x,120,0.05,1,0.2)

    # plt.plot(x, calculateBScall(x,120,0.05,0.001,0.1))
    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(80,150)
    ax.set_ylim(-5,75)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Option value')
    ax.set_title('Call option values for different time to maturity (volatility = 20%)')
    ax.legend()

    plt.savefig('call_option_20.png')

    plt.show()

def plotCallDeltas20():

    x = np.linspace(30,300, num=2700)
    y_001 = calculateDeltaCall(x,120,0.05,0.001,0.2)
    y_05 = calculateDeltaCall(x,120,0.05,0.5,0.2)
    y_1 = calculateDeltaCall(x,120,0.05,1,0.2)

    # plt.plot(x, calculateBScall(x,120,0.05,0.001,0.1))
    fig, ax = plt.subplots(figsize=(6,4))

    ax.plot(x, y_001, label='T=0.001')
    ax.plot(x, y_05, label='T=0.5')
    ax.plot(x, y_1, label='T=1')
    ax.set_xlim(80,150)
    ax.set_ylim(-0.2,2)
    ax.set_xlabel('Stock price')
    ax.set_ylabel('Option value')
    ax.set_title('Deltas of a call option for different time to maturity (volatility = 20%)')
    ax.legend()

    plt.savefig('call_delta_20.png')

    plt.show()

def binaryContract(initPrice, K, drift, time, volatility):
    d_1 = calculateD_1(initPrice, K, drift, time, volatility)
    value = (math.exp((-1)*drift*time)) * norm.cdf(d_1 - (volatility * math.sqrt(time)))
    return value

def printBinaryContract(initPrice, K, drift, time, volatility):
    print("Parameters for the calculation:")
    print(f'Initial stock price: {initPrice}')
    print(f'K: {K}')
    print(f'Risk free rate: {drift}')
    print(f'Time to maturity: {time}')
    print(f'Volatility: {volatility}')
    value = binaryContract(initPrice, K, drift, time, volatility)
    print(f'Calculated value of the option is: {round(value, 3)}')
    print('\n\n')


if (__name__ == "__main__") :
    #plotCallValues()
    # plotPutValues()
    # plotCallDeltas()
    #plotPutDeltas()
    #plotCallValues20()
    #plotCallDeltas20()
    printBinaryContract(60,50,0.05,1,0.2)
    printBinaryContract(100,120,0.1,0.5,0.1)