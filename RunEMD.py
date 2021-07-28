from EvolutionaryModelDiscovery import *
import numpy as np
modelPath = "SimpleSchellingTwoSubgroups_Hatna.nlogo"
setup = [
    'set neighborhood-distance 1',
    'set prob-of-relocation-attempt-by-happy random 0.01',
    'set fraction-of-blue 0.5',
    'set density 0.5 + (0.05 * random 4)',
    'set tolerance-dist-A 0.5 + (0.05 * random 4)',
    'set history-length 10',
    'setup']
measurements = ["c-index"]
ticks = 10
emd = EvolutionaryModelDiscovery("/home/social-sim/NetLogo 6.2.0/", modelPath,setup, measurements, ticks)
emd.setMutationRate(0.1)
emd.setCrossoverRate(0.8)
emd.setGenerations(20)
emd.setReplications(5)
emd.setDepth(2,6)
emd.setPopulationSize(20)
emd.setIsMinimize(False)

def cindexObjective(results):
    #print(results.iloc[-1][0])
    return np.mean(results.iloc[-1])

emd.setObjectiveFunction(cindexObjective)

if __name__ == '__main__':
    emd.evolve()
    