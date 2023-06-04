import pandas as pd
import numpy as np
import copy
import pyreadr

def uniqAinB(test, series):
#returns thruth value if test is the only value present in values
#e.g. filterUnique(values='abc, abc', test='abc') returns True.
#but 'abc, abc'=='abc' returns False
    insUnique = []
    for value in series:
        if type(value)==str:
            insUnique.append((test in value)
                    and
                    ([x for x in value.split(', ') if not(x==test)] == []))
        else:
            insUnique.append(False)
    return np.array(insUnique)

def filtAinB(test, series):
#returns truth value array if test or any x in test is in value
    ins = []
    for value in series:
        if type(value)==str:
            if type(test)==str:
                ins.append(test in value)
            else:
                test = list(test)
                ins.append([x for x in test if x in value] != [])
        else:
            ins.append(False)

    return np.array(ins)

def hasInfo(series):
    r = []
    for s in series:
        if type(s) == str:
            r.append(True)
        elif type(s) == int:
            r.append(True)
        elif type(s) == float:
            r.append(not(np.isnan(s)))
        else:
            r.append(False)

    return np.array(r)


def outcrossing(df):
    boolsList = []
    #-------------------------------
    #Is obligatory cross acc to ecoFlora
    obligatoryCrossers = uniqAinB('obligatory cross', df['Fertilization'])
    boolsList.append(obligatoryCrossers)
    #-------------------------------
    insectsFert = uniqAinB('insects', df['Fertilization'])
    boolsList.append(insectsFert)
    #-------------------------------
    selfSterileFert = filtAinB('self sterile', df['Fertilization'])
    boolsList.append(selfSterileFert)
    #------------------------------
    #fix dioecous1
    dioecous2 = uniqAinB('dioecous', df['Dicliny'])
    boolsList.append(dioecous2)
    #-------------------------------
    #dichogamy
    dichogamousEnough = []
    for dichog in df['Dichogamy']:
        dichogamousEnough.append(   (dichog=='protogynous ')            or
                                    (dichog=='protandrous')             or
                                    (dichog=='markedly protandrous')    or
                                    (dichog=='markedly protogynous')    or
                                    (dichog=='entirely protandrous')    or
                                    (dichog=='entirely protogynous '))
    boolsList.append(dichogamousEnough)
    #-------------------------------
    #self incompatibility
    selfIncompatible = []
    for incomp in df['Incompatibility systems']:
        if type(incomp)==str:
            selfIncompatible.append(not('none' in incomp))
        else:
            selfIncompatible.append(False)
    boolsList.append(selfIncompatible)
    #-------------------------------
    #is poacae
    #poacae = []
    #for species in df['species']:
    #    poacae.append(species.split('_')[0]=='poa')
    #boolsList.append(poacae)
    #-----------

    boolsArray = np.array(boolsList)
    return(np.any(boolsArray, axis=0))

def cleistogamous(df):
    cleists=[]
    for cleist in df['Cleistogamy']:
        if type(cleist)==str:
            #print(cleist)
            cleists.append( cleist=='pseudo-cleistogamous'     or
                            cleist=='entirely cleistogamous'    or
                            cleist=='usually cleistogamous')
        else:
            cleists.append(False)
    return np.array(cleists)

def assign3(fdf):
    df = copy.deepcopy(fdf)

    outcrossBool = outcrossing(df)
    cleistogamousBool = cleistogamous(df)
    apomicticBool = uniqAinB('apomictic', df['Fertilization'])
    mixedBool = filtAinB(['cross and self',
                            'cross or automatic self',
                            'normally cross',
                            'normally self'],
                            df['Fertilization'])
    selfBool = cleistogamousBool | apomicticBool
    mixedBool = mixedBool | (outcrossBool & selfBool)
    selfBool = selfBool & ~mixedBool & ~outcrossBool
    outcrossBool = outcrossBool & ~mixedBool

    #check
    #print(selfBool.sum() + mixedBool.sum() + outcrossBool.sum())
    #print('should eq')
    #print((selfBool | mixedBool | outcrossBool).sum())

    df['myFert3'] = np.nan
    df.loc[outcrossBool, 'myFert3'] = 'outcrossing'
    df.loc[selfBool, 'myFert3'] = 'selfing'
    df.loc[mixedBool, 'myFert3'] = 'mixed'

    return df

def assign5(fdf):
    df = copy.deepcopy(fdf)
    outcrossBool = outcrossing(df)
    cleistogamousBool = cleistogamous(df)
    apomicticBool = uniqAinB('apomictic', df['Fertilization'])
    normCrossBool = filtAinB('normally cross', df['Fertilization'])
    normSelfBool = filtAinB('normally self', df['Fertilization'])
    mixedBool = filtAinB(['cross and self',
                            'cross or automatic self',],
                            df['Fertilization'])

    selfBool = cleistogamousBool | apomicticBool
    normCrossBool = normCrossBool | (outcrossBool & mixedBool)
    normSelfBool = normSelfBool | (selfBool & mixedBool)
    mixedBool = mixedBool | (mixedBool & (selfBool | normSelfBool | normCrossBool | outcrossBool))
    mixedBool = mixedBool | (outcrossBool & selfBool)

    outcrossBool = outcrossBool & ~(selfBool | mixedBool | normSelfBool | normCrossBool)
    selfBool = selfBool & ~(normSelfBool | mixedBool | normCrossBool | outcrossBool)
    normCrossBool = normCrossBool & ~(selfBool | mixedBool | normSelfBool | outcrossBool)
    normSelfBool = normSelfBool & ~(selfBool | mixedBool | normCrossBool | outcrossBool)

    #print(selfBool.sum() + normSelfBool.sum() +  mixedBool.sum() + normCrossBool.sum() +  outcrossBool.sum())
    #print('should eq')
    #print((selfBool | normSelfBool | mixedBool | normCrossBool | outcrossBool).sum())

    df['myFert5'] = np.nan
    df.loc[selfBool, 'myFert5'] = 'selfing'
    df.loc[normSelfBool, 'myFert5'] = 'normally self'
    df.loc[mixedBool, 'myFert5'] = 'mixed'
    df.loc[normCrossBool, 'myFert5'] = 'normally cross'
    df.loc[outcrossBool, 'myFert5'] = 'outcrossing'

    return df

def assignHeavyMet(fdf):
    df = copy.deepcopy(fdf)
    noneBool = uniqAinB('none', df['Heavy metal resistance'])
    pseadoPhyteBool = filtAinB('pseudometallophyte,', df['Heavy metal resistance'])
    localPhyteBool = filtAinB('local metallophyte', df['Heavy metal resistance'])
    someBool = filtAinB('some', df['Heavy metal resistance'])
    absoluteBool = filtAinB('absolute metallophyte', df['Heavy metal resistance'])

    df['myHeavyMet'] = np.nan
    df.loc[noneBool, 'myHeavyMet'] = 'none'
    df.loc[pseadoPhyteBool, 'myHeavyMet'] = 'pseudometallophyte'
    df.loc[localPhyteBool, 'myHeavyMet'] = 'local metallophyte'
    df.loc[someBool, 'myHeavyMet'] = 'some'
    df.loc[absoluteBool, 'myHeavyMet'] = 'absolute metallophyte'

    return df


def assignPlantAtRange(fdf):
    df = copy.deepcopy(fdf)
    dfPlantAt = pd.read_csv('/home/sean/NERCflora/plantAtlas/sourceData.csv', sep = '|', na_values=['', 'nan', 'NaN', 'Nan'])
    plantAtSpecies = dfPlantAt.species.unique()

    df['myPlantAtRange'] = np.nan
    for species in df['species'].unique():
        if species in plantAtSpecies:
            gbRange = dfPlantAt[dfPlantAt['species']==species].iloc[0]['GB']
            try:
                gbRange = float(gbRange)
            except ValueError:
                gbRange = 0.0

            irRange = dfPlantAt[dfPlantAt['species']==species].iloc[0]['IR']
            try:
                irRange = float(irRange)
            except ValueError:
                irRange = 0.0

            ciRange = dfPlantAt[dfPlantAt['species']==species].iloc[0]['CI']
            try:
                ciRange = float(ciRange)
            except ValueError:
                ciRange = 0.0

            totalRange = gbRange + irRange + ciRange
            if totalRange!=0.0:
                df.loc[df['species']==species, 'myPlantAtRange']=totalRange

    return df

def assignPlantAtRarity(fdf):
    df = copy.deepcopy(fdf)
    dfPlantAt = pd.read_csv('/home/sean/NERCflora/plantAtlas/sourceData.csv', sep = '|', na_values=['', 'nan', 'NaN', 'Nan'])
    plantAtSpecies = dfPlantAt.species.unique()

    df['myPlantAtRarity'] = np.nan
    for species in df['species'].unique():
        if species in plantAtSpecies:
            rarity = dfPlantAt[dfPlantAt['species']==species].iloc[0]['RS']
            df.loc[df['species']==species, 'myPlantAtRarity']=rarity

    return df

def assignCombinedRarity(fdf):
    df = copy.deepcopy(fdf)
    df['ecoFloraRarity_copy'] = df['Rarity Status']
    #coding like plant atlas
    df.loc[df['ecoFloraRarity_copy']=='Present', 'ecoFloraRarity_copy'] = 'n'
    df.loc[df['ecoFloraRarity_copy']=='n, Present', 'ecoFloraRarity_copy'] = 'n'
    df.loc[df['ecoFloraRarity_copy']=='Scarce', 'ecoFloraRarity_copy'] = 's'
    df.loc[df['ecoFloraRarity_copy']=='Rare', 'ecoFloraRarity_copy'] = 'r'
    df.loc[df['ecoFloraRarity_copy']=='Apparently Extinct', 'ecoFloraRarity_copy'] = 'x'
    df.loc[df['ecoFloraRarity_copy']=='Insufficient Data', 'ecoFloraRarity_copy'] = 'i'

    speciesList = df.species.unique()
    df['myRarityCombined'] = df['myPlantAtRarity']
    for species in speciesList:
    #if species doesnt have rarity info from plantAt and does
    # have info from ecoFlora, then add in the info from ecoFlora
        if len(df[ (df['species']==species) & (hasInfo(df['myPlantAtRarity']))]['species']) == 0:
            if len(df[ (df['species']==species) & (hasInfo(df['ecoFloraRarity_copy']))]['species']) > 0:
                ecoFloraRarity = df[df['species']==species].iloc[0]['ecoFloraRarity_copy']
                df.loc[df['species']==species, 'myRarityCombined']=ecoFloraRarity
    del df['ecoFloraRarity_copy']
    #no rarity data
    df.loc[df['myRarityCombined']=='i','myRarityCombined']=np.nan

    return df

def assignPlantAtEllenberg(fdf):
    df = copy.deepcopy(fdf)
    df['myEBergL'] = np.nan
    df['myEBergF'] = np.nan
    df['myEBergR'] = np.nan
    df['myEBergN'] = np.nan
    df['myEBergS'] = np.nan

    dfPlantAt = pd.read_csv('/home/sean/NERCflora/plantAtlas/sourceData.csv', sep = '|', na_values=['', 'nan', 'NaN', 'Nan'])
    plantAtSpecies = dfPlantAt.species.unique()
    speciesList = df.species.unique()
    for species in speciesList:
        if species in plantAtSpecies:
            eBergL = dfPlantAt[dfPlantAt['species']==species].iloc[0]['L']
            eBergF = dfPlantAt[dfPlantAt['species']==species].iloc[0]['F']
            eBergR = dfPlantAt[dfPlantAt['species']==species].iloc[0]['R']
            eBergN = dfPlantAt[dfPlantAt['species']==species].iloc[0]['N']
            eBergS = dfPlantAt[dfPlantAt['species']==species].iloc[0]['S']

            df.loc[df['species']==species,'myEBergL'] = eBergL
            df.loc[df['species']==species,'myEBergF'] = eBergF
            df.loc[df['species']==species,'myEBergR'] = eBergR
            df.loc[df['species']==species,'myEBergN'] = eBergN
            df.loc[df['species']==species,'myEBergS'] = eBergS

    return df

def assignLocalRarity(fdf):
    df = copy.deepcopy(fdf)
    df['myLocalRarity'] = np.nan
    speciesList = df.species.unique()
    for species in speciesList:
        localRarity = df[df['species']==species].iloc[0]['Typical abundance where naturally occurring']
        if localRarity=='dominant, dominant':
            localRarity = 'dominant'
        if localRarity=='dominant, frequent':
            localRarity = 'frequent'
        if localRarity=='frequent, scattered':
            localRarity = 'scattered'
        if localRarity=='frequent, frequent':
            localRarity='frequent'
        if localRarity=='dominant, scattered':
            localRarity = 'frequent'
        if localRarity=='scattered, scattered':
            localRarity='scattered'
        if localRarity=='widespread':
            localRarity='dominant'

        df.loc[df['species']==species, 'myLocalRarity'] = localRarity

    return df


def assignPlantAtOthers(fdf):
    df = copy.deepcopy(fdf)
    speciesList = df.species.unique()
    dfPlantAt = pd.read_csv('/home/sean/NERCflora/plantAtlas/sourceData.csv', sep = '|', na_values=['', 'nan', 'NaN', 'Nan'])
    plantAtSpecies = dfPlantAt.species.unique()
    others = [  ['myPlantAtNativeStatus', 'NS'],
                ['myPlantAtConservationStatus', 'CS'],
                ['myPlantAtChangeIndex', 'Chg'],
                ['myPlantAtHeight', 'Hght'],
                ['myPlantAtLen', 'Len'],
                ['myPlantAtPern1', 'P1'],
                ['myPlantAtPern2', 'P2'],
                ['myPlantAtLife1', 'LF1'],
                ['myPlantAtLife2', 'LF2'],
                ['myPlantAtWood', 'W'],
                ['myPlantAtClone1', 'Clone1'],
                ['myPlantAtClone2', 'Clone2'],
                ['myPlantAtMajorBiome', 'E1'],
                ['myPlantAtEastLim', 'E2'],
                ['myPlantAtCont', 'C'],
                ['myPlantAtTjan', 'Tjan'],
                ['myPlantAtTjul', 'Tjul'],
                ['myPlantAtPrecip', 'Prec']
            ]

    for trait in others:
        name = trait[0]
        df[name] = np.nan

    for species in speciesList:
        if species in plantAtSpecies:
            for trait in others:
                name, code = trait
                value = dfPlantAt[dfPlantAt['species']==species].iloc[0][code]
                df.loc[df['species']==species,name]=value

    return df

def assign3General(fdf):
    df = copy.deepcopy(fdf)
    df['myFertGen'] = np.nan
    df.loc[     (df['myFert5']=='selfing')     |
                (df['myFert5']=='normally self'), 'myFertGen'] = 'generally self'
    df.loc[df['myFert5']=='mixed', 'myFertGen'] = 'mixed'
    df.loc[     (df['myFert5']=='outcrossing')  |
                (df['myFert5']=='normally cross'), 'myFertGen'] = 'generally cross'

    return df


def main():
    ecoFlora = pd.read_csv('/home/sean/NERCflora/ecoFlora/dataFlat.csv', sep='|')
    ecoFlora = assign3(ecoFlora)
    ecoFlora = assign5(ecoFlora)
    ecoFlora = assign3General(ecoFlora)
    ecoFlora = assignHeavyMet(ecoFlora)
    ecoFlora = assignPlantAtRange(ecoFlora)
    ecoFlora = assignPlantAtRarity(ecoFlora)
    ecoFlora = assignCombinedRarity(ecoFlora)
    ecoFlora = assignPlantAtEllenberg(ecoFlora)
    ecoFlora = assignPlantAtOthers(ecoFlora)
    ecoFlora = assignLocalRarity(ecoFlora)

def old_methods_interface_to_R(df_in):
    ecoFlora = df_in
    ecoFlora = assign3(ecoFlora)
    #ecoFlora = assign5(ecoFlora)
    #ecoFlora = assign3General(ecoFlora)
    #ecoFlora = assignHeavyMet(ecoFlora)
    #ecoFlora = assignPlantAtRange(ecoFlora)
    #ecoFlora = assignPlantAtRarity(ecoFlora)
    #ecoFlora = assignCombinedRarity(ecoFlora)
    #ecoFlora = assignPlantAtEllenberg(ecoFlora)
    #ecoFlora = assignPlantAtOthers(ecoFlora)
    #ecoFlora = assignLocalRarity(ecoFlora)
    return(ecoFlora)


new_data = pyreadr.read_r("data/new_data_scrape.rds")
new_data = pd.DataFrame(new_data[None])
new_data = new_data.rename(columns = {"main_species_name":"species"})
new_data_old_process = old_methods_interface_to_R(new_data)
