from EvolutionaryModelDiscovery import *
import numpy as np
modelPath = "SimpleSchellingTwoSubgroups_HatnaAdaption.nlogo"
setup = [
    'set neighborhood-distance 1',
    'set empty-cells-to-evaluate-frac 1',
    'set prob-of-relocation-attempt-by-happy random 0.01',
    'set fraction-of-blue 0.5',
    'set density 0.5 + (0.05 * random 10)',
    'set tolerance-dist-blue "0.125,0.5\\n0.833,0.5"',
    'set tolerance-dist-green "0.125,0.5\\n0.833,0.5"',
    'setup']
measurements = ["c-index"]
ticks = 100
emd = EvolutionaryModelDiscovery("/opt/netlogo/", modelPath,setup, measurements, ticks)
emd.setMutationRate(0.1)
emd.setCrossoverRate(0.8)
emd.setGenerations(50)
emd.setReplications(5)
emd.setDepth(2,6)
emd.setPopulationSize(50)
emd.setIsMinimize(False)

def cindexObjective(results):
    #print(results.iloc[-1][0])
    return np.mean(results.iloc[-1])

emd.setObjectiveFunction(cindexObjective)

if __name__ == '__main__':
    import warnings
    warnings.filterwarnings("ignore", category=DeprecationWarning) 
    emd.evolve()
    