def getPerforceChanges(fileName, branch):
    depoPath = '//mw/' + branch + '/' + fileName

def getRelevantBranches(fileName, branch, visitedBranches):

    visitedBranches.add(branch)

    otherBranches = set()

    changes = getPerforceChanges(fileName, branch)
    for change in changes:
        if change.isChangeFromAnotherBranch:
            if change.otherBranch not in visitedBranches:
                otherBranches.append(change.otherBranch)

    for otherBranch in otherBranches:
        getRelevantBranches(fileName, otherBranch, visitedBranches)




